-module(ami_client).
-export([start_link/0, connect/0, send_command/2, login/2, originate/1, sip_reload/0, sip_show_registry/0, decode_packet/1]).

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-record(state, {sock, connected, state, reply_queue, packet}).

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, {}, []).

connect() ->
  gen_server:cast(?SERVER, connect).

login(Username, Secret) ->
  send_command("Login", [{"Username", Username}, {"Secret", Secret}]).

originate(Parameters) ->
  send_command("Originate", Parameters).

sip_reload() ->
  send_command("Command", [{command, "sip reload"}]).

sip_show_registry() ->
  send_command("sipshowregistry", []).

send_command(Action, Parameters) ->
  gen_server:call(?SERVER, {send_command, Action, Parameters}).

decode_packet(Packet) -> decode_packet(Packet, []).
decode_packet([], Decoded) -> Decoded;
decode_packet([Line | Rest], Decoded) ->
  [ParamName, ParamValue] = binary:split(Line, [<<": ">>, <<":">>]),
  NewDecoded = [{util:binary_to_lower_atom(ParamName), ParamValue} | Decoded],
  decode_packet(Rest, NewDecoded).

%% @private
init({}) ->
  {ok, #state{connected = false}}.

%% @private
handle_call({send_command, _, _}, _, State = #state{connected = false}) ->
  {reply, {error, unavailable}, State};

handle_call({send_command, Action, Parameters}, From, State = #state{sock = Sock, reply_queue = Queue}) ->
  case gen_tcp:send(Sock, build_command(Action, Parameters)) of
    ok -> {noreply, State#state{reply_queue = queue:in(From, Queue)}};
    _ ->
      gen_tcp:close(Sock),
      gen_server:cast(?SERVER, connect),
      {reply, {error, unavailable}, #state{connected = false}}
  end;

handle_call(_Request, _From, State) ->
  {reply, {error, unknown_call}, State}.

%% @private
handle_cast(connect, State = #state{connected = false}) ->
  case gen_tcp:connect("localhost", 5038, [binary, {packet, line}], 1000) of
    {ok, Sock} ->
      ami_events:notify_event(connected),
      {noreply, #state{sock = Sock, connected = true, state = initial, reply_queue = queue:new()}};
    _ ->
      {ok, _} = timer:apply_after(1000, gen_server, cast, [?SERVER, connect]),
      {noreply, State}
  end;

handle_cast(_Msg, State) ->
  {noreply, State}.

%% @private
handle_info({tcp, _, _}, State = #state{state = initial}) ->
  {noreply, State#state{state = waiting}};

handle_info({tcp, _, <<"\r\n">>}, State = #state{state = receiving, reply_queue = Queue, packet = Packet}) ->
  case Packet of
    {response, Response, Data} ->
      case queue:out(Queue) of
        {{value, From}, NewQueue} ->
          gen_server:reply(From, {Response, Data}),
          {noreply, State#state{state = waiting, reply_queue = NewQueue, packet = undefined}};
        _ ->
          {noreply, State#state{state = waiting, packet = undefined}}
      end;
    {event, Event, Data} ->
      ami_events:notify_event({Event, Data}),
      {noreply, State#state{state = waiting, packet = undefined}}
  end;

handle_info({tcp, _, Line}, State) ->
  handle_line(util:strip_nl(Line), State);

handle_info({tcp_closed, _}, _State) ->
  gen_server:cast(?SERVER, connect),
  {noreply, #state{connected = false}};

handle_info(_Info, State) ->
  {noreply, State}.

%% @private
terminate(_Reason, _State) ->
  ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

handle_line(<<"Response: ", Response/binary>>, State = #state{state = waiting}) ->
  Packet = {response, util:binary_to_lower_atom(Response), []},
  {noreply, State#state{state = receiving, packet = Packet}};

handle_line(<<"Event: ", Event/binary>>, State = #state{state = waiting}) ->
  Packet = {event, util:binary_to_lower_atom(Event), []},
  {noreply, State#state{state = receiving, packet = Packet}};

handle_line(Line, State = #state{state = receiving, packet = {PacketType, Value, Params}}) ->
  NewPacket = {PacketType, Value, [Line | Params]},
  {noreply, State#state{packet = NewPacket}}.

build_command(Action, Parameters) ->
  ["Action: ", Action, "\r\n", build_parameters(Parameters), "\r\n"].

build_parameters([]) -> [];
build_parameters([{Key, Value} | Rest]) ->
  [util:to_string(Key), ": ", util:to_string(Value), "\r\n" | build_parameters(Rest)].
