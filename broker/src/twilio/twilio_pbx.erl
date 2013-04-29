-module(twilio_pbx).
-export([answer/1, hangup/1, can_play/2, play/2, capture/6, terminate/1, sound_path_for/2]).
-behaviour(pbx).

-export([start_link/1, find/1, new/1, await_response/1, resume/1]).

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(PBX(Pid), {?MODULE, Pid}).
-define(PBX, ?PBX(Pid)).

-record(state, {awaiter, session, response = <<>>}).

start_link(CallSid) ->
  gen_server:start_link({global, {twilio_pbx, CallSid}}, ?MODULE, {}, []).

find(CallSid) ->
  case global:whereis_name({twilio_pbx, CallSid}) of
    undefined -> undefined;
    Pid -> {?MODULE, Pid}
  end.

new(CallSid) ->
  {ok, Pid} = twilio_pbx_sup:start_session(CallSid),
  {?MODULE, Pid}.

answer(?PBX(_)) -> ok.

hangup(?PBX(_)) -> throw(not_implemented).

can_play(url, _) -> true;
can_play(text, _) -> true;
can_play(file, _) -> true.

play(Resource, ?PBX) ->
  gen_server:call(Pid, {play, Resource}, timer:minutes(5)).

capture(_, _, _, _, _, ?PBX(_)) -> throw(not_implemented).

terminate(?PBX) ->
  gen_server:call(Pid, terminate).

sound_path_for(Name, ?PBX(_)) ->
  "/usr/local/asterisk/var/lib/asterisk/sounds/verboice/" ++ Name ++ ".gsm".

await_response(?PBX) ->
  gen_server:call(Pid, await_response).

resume(?PBX) ->
  gen_server:call(Pid, resume).

%% @private
init({}) ->
  {ok, #state{}}.

%% @private
handle_call(await_response, From, State) ->
  {noreply, State#state{awaiter = From}};

handle_call(resume, From, State = #state{session = Session}) ->
  gen_server:reply(Session, ok),
  {reply, ok, State = #state{session = undefined}};

handle_call({play, {text, Text}}, From, State) ->
  flush(From, append(<<"<Say>", Text/binary, "</Say>">>, State));

handle_call(terminate, _From, State) ->
  {stop, normal, ok, State};

handle_call(_Request, _From, State) ->
  {reply, {error, unknown_call}, State}.

%% @private
handle_cast(_Msg, State) ->
  {noreply, State}.

%% @private
handle_info(_Info, State) ->
  {noreply, State}.

%% @private
terminate(_Reason, #state{awaiter = Awaiter}) when Awaiter =/= undefined ->
  gen_server:reply(Awaiter, "<Response><Hangup/></Response>");

terminate(_Reason, _State) ->
  ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

append(Command, State = #state{response = Response}) ->
  State#state{response = <<Response/binary, Command/binary>>}.

flush(From, #state{awaiter = Awaiter, response = Response}) ->
  gen_server:reply(Awaiter, binary_to_list(<<"<Response>", Response/binary, "</Response>">>)),
  {noreply, #state{session = From}}.

