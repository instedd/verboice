-module(twilio_pbx).
-export([pid/1, answer/1, hangup/1, can_play/2, play/2, capture/6, terminate/1, sound_path_for/2, dial/4]).
-behaviour(pbx).

-export([start_link/2, find/1, new/2, resume/2]).

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(PBX(Pid), {?MODULE, Pid}).
-define(PBX, ?PBX(Pid)).

-record(state, {callback_url, awaiter, session, commands = [], waiting}).

start_link(CallSid, CallbackUrl) ->
  gen_server:start_link({global, {twilio_pbx, CallSid}}, ?MODULE, CallbackUrl, []).

find(CallSid) ->
  case global:whereis_name({twilio_pbx, CallSid}) of
    undefined -> undefined;
    Pid -> {?MODULE, Pid}
  end.

new(CallSid, CallbackUrl) ->
  {ok, Pid} = twilio_pbx_sup:start_session(CallSid, CallbackUrl),
  {?MODULE, Pid}.

pid(?PBX) -> Pid.

answer(?PBX(_)) -> ok.

hangup(?PBX(_)) -> throw(not_implemented).

can_play(url, _) -> true;
can_play({text, Lang}, _) -> lists:member(Lang, ["en", "es", "fr", "de", "it"]);
can_play(file, _) -> true.

play(Resource, ?PBX) ->
  gen_server:call(Pid, {play, Resource}, timer:minutes(5)).

capture(Caption, Timeout, FinishOnKey, Min, Max, ?PBX(Pid)) ->
  case gen_server:call(Pid, {capture, Caption, Timeout, FinishOnKey, Min, Max}, timer:minutes(5)) of
    hangup -> throw(hangup);
    X -> X
  end.

terminate(?PBX) ->
  catch gen_server:call(Pid, terminate).

sound_path_for(Name, ?PBX(_)) ->
  {ok, Dir} = file:get_cwd(),
  filename:join([Dir, "tmp/www", Name ++ ".mp3"]).

dial(_, _, _, _) -> exit(unimplemented).

resume(Params, ?PBX) ->
  gen_server:call(Pid, {resume, Params}).

%% @private
init(CallbackUrl) ->
  {ok, #state{callback_url = CallbackUrl}}.

%% @private
handle_call({resume, _Params}, From, State = #state{session = undefined}) ->
  {noreply, State#state{awaiter = From}};

handle_call({resume, Params}, From, State = #state{session = Session, waiting = {capture, Min}}) ->
  Reply = case proplists:get_value("Digits", Params) of
    undefined -> timeout;
    Digits ->
      if
        length(Digits) < Min -> short_entry;
        true -> {digits, Digits}
      end
  end,
  gen_server:reply(Session, Reply),
  {noreply, State#state{awaiter = From, waiting = undefined}};

handle_call({resume, _Params}, From, State = #state{session = Session}) ->
  gen_server:reply(Session, ok),
  {noreply, State#state{awaiter = From}};

handle_call({play, Resource}, _From, State) ->
  {reply, ok, append(resource_command(Resource, State), State)};

handle_call({capture, Resource, Timeout, FinishOnKey, Min, Max}, From, State) ->
  Command =
    {'Gather', [{timeout, Timeout}, {finishOnKey, FinishOnKey}, {numDigits, Max}],
      [resource_command(Resource, State)]
    },
  flush(From, append_with_callback(Command, State#state{waiting = {capture, Min}}));

handle_call(terminate, _From, State) ->
  {stop, normal, ok, State}.

%% @private
handle_cast(_Msg, State) ->
  {noreply, State}.

%% @private
handle_info(_Info, State) ->
  {noreply, State}.

%% @private
terminate(_Reason, State) ->
  case State#state.awaiter of
    undefined -> ok;
    _ -> flush(nobody, append('Hangup', State))
  end,
  case State#state.session of
    undefined -> ok;
    Session -> gen_server:reply(Session, hangup)
  end.

%% @private
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

resource_command({text, Language, Text}, _) ->
  {'Say', [{language, Language}], [binary_to_list(Text)]};

resource_command({file, Name}, #state{callback_url = CallbackUrl}) ->
  {'Play', [[CallbackUrl, Name, ".mp3"]]}.

append(Command, State = #state{commands = Commands}) ->
  State#state{commands = [Command | Commands]}.

append_with_callback(Command, State = #state{callback_url = CallbackUrl}) ->
  append({'Redirect', [CallbackUrl]}, append(Command, State)).

flush(From, State = #state{awaiter = Awaiter, commands = Commands}) ->
  Response = {'Response', lists:reverse(Commands)},
  ResponseXml = binary_to_list(iolist_to_binary(xmerl:export_simple([Response], xmerl_xml))),
  gen_server:reply(Awaiter, ResponseXml),
  {noreply, State#state{session = From, awaiter = undefined, commands = []}}.

