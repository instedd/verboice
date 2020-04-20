-module(africas_talking_pbx).
-compile([{parse_transform, lager_transform}]).
-export([pid/1, answer/1, reject/1, hangup/1, can_play/2, play/2, capture/6, terminate/1, sound_path_for/2, sound_quality/1, dial/6, record/5]).
-behaviour(pbx).

-export([start_link/1, find/1, new/1, resume/2, user_hangup/1]).

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(PBX(Pid), {?MODULE, Pid}).
-define(PBX, ?PBX(Pid)).

-record(state, {callback_url, awaiter, session, commands = [], waiting}).

-include("uri.hrl").

start_link(CallSid) ->
  gen_server:start_link({global, {africas_talking_pbx, CallSid}}, ?MODULE, {}, []).

find(CallSid) ->
  case global:whereis_name({africas_talking_pbx, CallSid}) of
    undefined -> undefined;
    Pid -> {?MODULE, Pid}
  end.

new(CallSid) ->
  {ok, Pid} = africas_talking_pbx_sup:start_session(CallSid),
  {?MODULE, Pid}.

pid(?PBX) -> Pid.

answer(?PBX(_)) -> ok.

reject(?PBX) ->
  gen_server:call(Pid, reject).

hangup(?PBX) ->
  gen_server:call(Pid, hangup).

can_play(url, _) -> true;
can_play({text, Lang}, _) -> lists:member(util:to_string(Lang), ["en", "es", "fr", "de", "it"]);
can_play(file, _) -> true.

play(Resource, ?PBX) ->
  gen_server:call(Pid, {play, Resource}, timer:minutes(5)).

capture(Caption, Timeout, FinishOnKey, Min, Max, ?PBX(Pid)) ->
  case gen_server:call(Pid, {capture, Caption, Timeout, FinishOnKey, Min, Max}, timer:minutes(5)) of
    hangup -> throw(hangup);
    X -> X
  end.

record(_AsteriskFileName, FileName, StopKeys, Timeout, ?PBX(Pid)) ->
  case gen_server:call(Pid, {record, FileName, StopKeys, Timeout}, timer:minutes(5)) of
    hangup -> throw(hangup);
    X -> X
  end.

terminate(?PBX) ->
  catch gen_server:call(Pid, terminate).

sound_path_for(Name, ?PBX(_)) ->
  {ok, Dir} = file:get_cwd(),
  filename:join([Dir, "tmp/www", Name ++ ".mp3"]).

sound_quality(?PBX(_)) ->
  "44100".

dial(_Channel, Number, CallerId, _LocalFilename, _AsteriskFilename, ?PBX(Pid)) ->
  % FIXME: implement session recording
  case gen_server:call(Pid, {dial, Number, CallerId}, timer:minutes(60)) of
    hangup -> throw(hangup) ;
    X -> X
  end.

resume(Params, ?PBX) ->
  try
    gen_server:call(Pid, {resume, Params})
  catch
    exit:{timeout, _} -> terminate(?PBX)
  end.

user_hangup(?PBX) ->
  gen_server:call(Pid, user_hangup).

%% @private
init({}) ->
  CallbackUrl = verboice_config:broker_httpd_base_url() ++ "africas_talking",
  {ok, #state{callback_url = CallbackUrl}, 1000}.

%% @private
numDigitsWithoutInfinity(infinity) -> 99;
% ".inf" param is received whenever max < min and the flow that was generated using the Verboice Designer
numDigitsWithoutInfinity(".inf") -> 99;
numDigitsWithoutInfinity(Number) -> Number.

%% @private
handle_call({resume, _Params}, From, State = #state{session = undefined}) ->
  {noreply, State#state{awaiter = From}};

handle_call({resume, Params}, From, State = #state{session = Session, waiting = {capture, Min}}) ->
  Reply = case proplists:get_value("dtmfDigits", Params) of
    undefined -> timeout;
    Digits ->
      if
        length(Digits) < Min -> short_entry;
        true -> {digits, Digits}
      end
  end,
  gen_server:reply(Session, Reply),
  {noreply, State#state{awaiter = From, waiting = undefined}};


handle_call({resume, Params}, From, State = #state{session = Session, waiting = {record, FileName}}) ->
  Reply = case proplists:get_value("RecordingUrl", Params) of
    undefined -> timeout;
    RecordingUrl ->
      RequestUri = uri:parse(RecordingUrl),
      retrieve_recording_with_retries(RequestUri, FileName, 3)
  end,
  gen_server:reply(Session, Reply),
  {noreply, State#state{awaiter = From, waiting = undefined}};

handle_call({resume, Params}, From, State = #state{session = Session, waiting = dial}) ->
  Reply = case proplists:get_value("DialCallStatus", Params) of
    "completed" -> completed;
    "busy" -> busy;
    "no-answer" -> no_answer;
    "failed" -> failed;
    "canceled" -> cancelled;
    Other ->
      lager:warning("Unknown DialCallStatus: ~p", [Other]),
      failed
  end,
  gen_server:reply(Session, Reply),
  {noreply, State#state{awaiter = From, waiting = undefined}};

handle_call({resume, _Params}, From, State = #state{session = Session}) ->
  gen_server:reply(Session, ok),
  {noreply, State#state{awaiter = From}};

handle_call({play, Resource}, _From, State) ->
  {reply, ok, append(resource_command(Resource, State), State)};

handle_call({capture, Resource, Timeout, [], Min, Max}, From, State) ->
  Command = {'GetDigits', [{timeout, Timeout}, {numDigits, numDigitsWithoutInfinity(Max)}], [resource_command(Resource, State)]},
  flush(From, append_with_callback(Command, State#state{waiting = {capture, Min}}));

handle_call({capture, Resource, Timeout, FinishOnKey, Min, Max}, From, State) ->
  Command = {'GetDigits', [{timeout, Timeout}, {finishOnKey, FinishOnKey}, {numDigits, numDigitsWithoutInfinity(Max)}], [resource_command(Resource, State)]},
  flush(From, append_with_callback(Command, State#state{waiting = {capture, Min}}));

handle_call({record, FileName, StopKeys, Timeout}, From, State) ->
  Command =
    {'Record', [{timeout, Timeout}, {finishOnKey, StopKeys}], []},
  flush(From, append_with_callback(Command, State#state{waiting = {record, FileName}}));

handle_call({dial, Number, _CallerId}, From, State) ->
  Command = {'Dial', [{phoneNumbers, "+" ++ binary_to_list(Number)}], []},
  flush(From, append(Command, State#state{waiting = dial}));

handle_call(user_hangup, _From, State) ->
  case State#state.session of
    undefined -> ok;
    Session -> gen_server:reply(Session, hangup)
  end,
  {reply, ok, State, timer:seconds(5)};

handle_call(hangup, _From, State) ->
  TerminateCmd = {'Say', [{language, "en"}], ["."]},
  {noreply, HangupState} = flush(undefined, append(TerminateCmd, State)),
  {reply, ok, HangupState};

handle_call(reject, _From, State) ->
  {noreply, RejectState} = flush(undefined, append('Reject', State)),
  {reply, ok, RejectState};

handle_call(terminate, _From, State) ->
  {stop, normal, ok, State}.

%% @private
handle_cast(_Msg, State) ->
  {noreply, State}.

%% @private
handle_info(timeout, State) ->
  {stop, timeout, State};

handle_info(_Info, State) ->
  {noreply, State}.

%% @private
terminate(_Reason, State) ->
  TerminateCmd = {'Say', [{language, "en"}], ["."]},
  case State#state.awaiter of
    undefined -> ok;
    _ -> flush(nobody, append(TerminateCmd, State))
  end.

%% @private
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

resource_command({text, Language, Text}, _) ->
  {'Say', [{language, Language}], [binary_to_list(Text)]};

resource_command({file, Name}, _) ->
  {'Play', [{url, [[verboice_config:broker_httpd_base_url(), Name, ".mp3"]]}], []};

resource_command({url, Url}, _) ->
  {'Play', [{url, [Url]}], []}.

append(Command, State = #state{commands = Commands}) ->
  State#state{commands = [Command | Commands]}.

append_with_callback(Command, State = #state{callback_url = CallbackUrl}) ->
  append({'Redirect', [CallbackUrl]}, append(Command, State)).

flush(From, State = #state{awaiter = Awaiter, commands = Commands}) ->
  Response = {'Response', lists:reverse(Commands)},
  ResponseXml = binary_to_list(iolist_to_binary(xmerl:export_simple([Response], xmerl_xml))),
  gen_server:reply(Awaiter, ResponseXml),
  {noreply, State#state{session = From, awaiter = undefined, commands = []}}.

retrieve_recording_with_retries(RequestUri, FileName, Retries) ->
  case uri:get([{body_format, binary}, {stream, FileName}], RequestUri) of
    {ok, saved_to_file} -> ok;
    {ok, {{_, 404, _}, _, _}} ->
      case Retries of
        0 -> {error, not_found};
        _ -> timer:sleep(200), retrieve_recording_with_retries(RequestUri, FileName, Retries - 1)
      end;
    Ret -> {error, Ret}
  end.
