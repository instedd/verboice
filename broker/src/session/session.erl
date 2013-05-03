-module(session).
-export([start_link/1, new/0, find/1, answer/2, answer/3, dial/4, reject/2, stop/1]).
-export([language/1]).

% FSM Description
% Possible states: ready, dialing, in_progress, suspended, completed
% Initial state: ready
%
% ready(dial) -> dialing
% ready(answer) -> in_progress
% dialing(answer) -> in_progress
% dialing(error | no_answer | busy) -> failed
% in_progress(error | hangup) -> failed
% in_progress(done) -> completed
% in_progress(suspend) -> suspended
% suspended(resume) -> in_progress

-behaviour(gen_fsm).
-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).
-export([ready/2, ready/3, dialing/2, in_progress/2]).

-define(SESSION(Id), {session, Id}).

-include("session.hrl").
-include("db.hrl").

start_link(SessionId) ->
  gen_fsm:start_link({global, ?SESSION(SessionId)}, ?MODULE, SessionId, []).

new() ->
  SessionId = erlang:ref_to_list(make_ref()),
  SessionSpec = {SessionId, {session, start_link, [SessionId]}, temporary, 5000, worker, [session]},
  supervisor:start_child(session_sup, SessionSpec).

find(SessionId) ->
  global:whereis_name(?SESSION(SessionId)).

answer(SessionPid, Pbx, ChannelId) ->
  gen_fsm:send_event(SessionPid, {answer, Pbx, ChannelId}).

answer(SessionPid, Pbx) ->
  gen_fsm:send_event(SessionPid, {answer, Pbx}).

dial(SessionPid, RealBroker, Channel, QueuedCall) ->
  gen_fsm:sync_send_event(SessionPid, {dial, RealBroker, Channel, QueuedCall}).

reject(SessionPid, Reason) ->
  gen_fsm:send_event(SessionPid, {reject, Reason}).

stop(SessionPid) ->
  gen_fsm:send_all_state_event(SessionPid, stop).

language(#session{js_context = JsContext}) ->
  erjs_object:get(var_language, JsContext).

%% @private

init(SessionId) ->
  {ok, ready, #session{session_id = SessionId}}.

ready({answer, Pbx, ChannelId}, Session) ->
  error_logger:info_msg("Session (~p) answer", [Session#session.session_id]),
  Channel = channel:find(ChannelId),
  CallFlow = call_flow:find(Channel#channel.call_flow_id),
  Project = project:find(CallFlow#call_flow.project_id),
  CallLog = call_log:create(#call_log{
    account_id = Channel#channel.account_id,
    project_id = CallFlow#call_flow.project_id,
    state = "active",
    direction = "incoming",
    channel_id = ChannelId,
    started_at = calendar:universal_time(),
    call_flow_id = CallFlow#call_flow.id
  }),
  Flow = CallFlow:commands(),
  io:format("~p~n", [Flow]),

  NewSession = Session#session{pbx = Pbx, flow = Flow, call_log = CallLog, project = Project},
  spawn_run(NewSession),

  {next_state, in_progress, NewSession}.

ready({dial, RealBroker, Channel, QueuedCall}, _From, Session) ->
  error_logger:info_msg("Session (~p) dial", [Session#session.session_id]),
  CallLog = call_log:find(QueuedCall#queued_call.call_log_id),
  Project = project:find(QueuedCall#queued_call.project_id),

  NewSession = Session#session{
    channel = Channel,
    address = QueuedCall#queued_call.address,
    call_log = CallLog,
    queued_call = QueuedCall,
    project = Project
  },

  case RealBroker:dispatch(NewSession) of
    {error, _} -> {stop, normal, unavailable, Session};
    _ ->
      CallLog:info(["Dialing to ", QueuedCall#queued_call.address, " through channel ", Channel#channel.name], []),
      notify_status(ringing, NewSession),
      {reply, ok, dialing, NewSession#session{call_log = call_log:update(CallLog#call_log{state = "active", fail_reason = undefined})}}
  end.

dialing({answer, Pbx}, Session) ->
  error_logger:info_msg("Session (~p) answer", [Session#session.session_id]),
  CallFlow = call_flow:find(Session#session.queued_call#queued_call.call_flow_id),
  NewSession = Session#session{pbx = Pbx, flow = CallFlow:commands()},
  notify_status('in-progress', NewSession),
  spawn_run(NewSession),

  {next_state, in_progress, NewSession};

dialing({reject, Reason}, Session = #session{session_id = SessionId, call_log = CallLog}) ->
  error_logger:info_msg("Session (~p) rejected, reason: ~p", [SessionId, Reason]),
  CallLog:error(["Call was rejected. (Reason: ", atom_to_list(Reason),")"], []),
  notify_status('no-answer', Session),
  finalize({failed, Reason}, Session);

dialing(timeout, Session) ->
  notify_status(busy, Session),
  {stop, timeout, Session}.

in_progress({completed, ok}, Session) ->
  notify_status('completed', Session),
  finalize(completed, Session);

in_progress({completed, {error, Reason}}, Session) ->
  notify_status('failed', Session),
  finalize({failed, Reason}, Session).

notify_status(Status, Session = #session{session_id = SessionId, address = Address}) ->
  Project = Session#session.project,
  case Project#project.status_callback_url of
    undefined -> ok;
    <<>> -> ok;
    Url ->
      StatusCallbackUrl = binary_to_list(Url),
      QueryString = "CallSid=" ++ http_uri:encode(SessionId) ++
                    "&CallStatus=" ++ atom_to_list(Status) ++
                    "&From=" ++ binary_to_list(Address),
      spawn(fun() -> httpc:request(get, {StatusCallbackUrl ++ "?" ++ QueryString, []}, [], [{full_result, false}]) end)
  end.

handle_event(stop, _, Session) ->
  {stop, normal, Session}.

handle_sync_event(_Event, _From, StateName, Session) ->
  {reply, ok, StateName, Session}.

%% @private
handle_info({'DOWN', _Ref, process, _Pid, Reason}, _, Session) ->
  {stop, Reason, Session};

handle_info(Info, _StateName, State) ->
  io:format("~p~n", [Info]),
  {noreply, State}.

%% @private
terminate(Reason, _, #session{session_id = Id}) ->
  error_logger:info_msg("Session (~p) terminated with reason: ~p", [Id, Reason]),
  ok.

%% @private
code_change(_OldVsn, StateName, State, _Extra) ->
  {ok, StateName, State}.

finalize(completed, Session = #session{call_log = CallLog}) ->
  NewCallLog = call_log:update(CallLog#call_log{state = "completed", finished_at = calendar:universal_time()}),
  {stop, normal, Session#session{call_log = NewCallLog}};

finalize({failed, Reason}, Session = #session{call_log = CallLog}) ->
  NewState = case Session#session.queued_call of
    undefined -> "failed";
    QueuedCall ->
      case QueuedCall:reschedule() of
        no_schedule -> failed;
        max_retries ->
          CallLog:error("Max retries exceeded", []),
          "failed";
        #queued_call{not_before = NotBefore} ->
          CallLog:info(["Call rescheduled to start at ", httpd_util:rfc1123_date(calendar:universal_time_to_local_time(NotBefore))], []),
          "queued"
      end
  end,
  NewCallLog = call_log:update(CallLog#call_log{state = NewState, fail_reason = Reason, finished_at = calendar:universal_time()}),
  {stop, normal, Session#session{call_log = NewCallLog}}.

spawn_run(Session) ->
  SessionPid = self(),
  spawn_monitor(fun() ->
    {Result, _NewSession} = run(Session),
    gen_fsm:send_event(SessionPid, {completed, Result})
  end).

default_language(Session = #session{project = Project}) ->
  Language = case Session#session.queued_call of
    undefined -> Project#project.default_language;
    #queued_call{variables = Vars} ->
      case proplists:get_value(<<"language">>, Vars) of
        undefined -> Project#project.default_language;
        <<>> -> Project#project.default_language;
        Lang -> Lang
      end
  end,
  io:format("Default language: ~p~n", [Language]),
  binary_to_list(Language).

run(Session = #session{pbx = Pbx}) ->
  JsContext = erjs_object:new([{var_language, default_language(Session)}]),
  RunSession = Session#session{js_context = JsContext},
  try run(RunSession, 1) of
    X -> X
  after
    Pbx:terminate()
  end.

run(Session = #session{flow = Flow}, Ptr) when Ptr > length(Flow) -> {ok, Session};
run(Session = #session{flow = Flow, call_log = CallLog}, Ptr) ->
  Command = lists:nth(Ptr, Flow),
  try eval(Command, Session) of
    {Action, NewSession} ->
      case Action of
        next ->
          run(NewSession, Ptr + 1);
        {goto, N} ->
          run(NewSession, N + 1);
        {exec, NewFlow} ->
          run(NewSession#session{flow = NewFlow}, 1);
        finish ->
          {ok, NewSession}
      end
  catch
    hangup ->
      CallLog:info("The user hang up", []),
      {{error, hangup}, Session};
    Class:Error ->
      CallLog:error(["Error ", io_lib:format("~p:~p", [Class, Error])], []),
      error_logger:error_msg("Error during session ~p, call log ~p: ~p:~p~n~p~n",
        [Session#session.session_id, CallLog#call_log.id, Class, Error, erlang:get_stacktrace()]),
      {{error, error}, Session}
  end.

eval(stop, Session) -> {finish, Session};
eval([Command, Args], Session) -> Command:run(Args, Session);
eval(Command, Session) -> Command:run(Session).
