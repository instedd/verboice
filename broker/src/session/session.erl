-module(session).
-export([start_link/1, new/0, find/1, answer/2, answer/3, dial/4, reject/2, stop/1]).

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
-export([ready/2, dialing/2, in_progress/2]).

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
  gen_fsm:send_event(SessionPid, {dial, RealBroker, Channel, QueuedCall}).

reject(SessionPid, Reason) ->
  gen_fsm:send_event(SessionPid, {reject, Reason}).

stop(SessionPid) ->
  gen_fsm:send_all_state_event(SessionPid, stop).

%% @private

init(SessionId) ->
  {ok, ready, #session{session_id = SessionId}}.

ready({answer, Pbx, ChannelId}, Session) ->
  error_logger:info_msg("Session (~p) answer", [Session#session.session_id]),
  Channel = channel:find(ChannelId),
  CallFlow = call_flow:find(Channel#channel.call_flow_id),
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

  NewSession = Session#session{pbx = Pbx, flow = Flow, call_log = CallLog},
  spawn_monitor(fun() -> run(NewSession) end),

  {next_state, in_progress, NewSession};

ready({dial, RealBroker, Channel, QueuedCall}, Session) ->
  error_logger:info_msg("Session (~p) dial", [Session#session.session_id]),
  CallLog = call_log:find(QueuedCall#queued_call.call_log_id),
  NewSession = Session#session{
    channel = Channel,
    address = QueuedCall#queued_call.address,
    call_log = CallLog,
    queued_call = QueuedCall
  },

  RealBroker:dispatch(NewSession),
  {next_state, dialing, NewSession}.

dialing({answer, Pbx}, Session) ->
  error_logger:info_msg("Session (~p) answer", [Session#session.session_id]),
  CallFlow = call_flow:find(Session#session.queued_call#queued_call.call_flow_id),
  NewSession = Session#session{pbx = Pbx, flow = CallFlow:commands()},
  spawn_monitor(fun() -> run(NewSession) end),

  {next_state, in_progress, NewSession};

dialing({reject, Reason}, Session = #session{session_id = SessionId}) ->
  error_logger:info_msg("Session (~p) rejected, reason: ~p", [SessionId, Reason]),
  {stop, normal, Session};

dialing(timeout, Session) ->
  {stop, timeout, Session}.

in_progress(done, Session = #session{call_log = CallLog}) ->
  NewCallLog = call_log:update(CallLog#call_log{state = "completed", finished_at = calendar:universal_time()}),
  {stop, normal, Session#session{call_log = NewCallLog}}.

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
terminate(_Reason, _, #session{session_id = Id}) ->
  error_logger:info_msg("Session (~p) terminated", [Id]),
  ok.

%% @private
code_change(_OldVsn, _StateName, State, _Extra) ->
  {ok, State}.

run(State = #session{pbx = Pbx}) ->
  JsContext = erjs_object:new(),
  RunState = State#session{js_context = JsContext},
  try run(RunState, 1) of
    _ ->
      gen_fsm:send_event({global, ?SESSION(State#session.session_id)}, done)
  catch
    hangup -> hangup;
    A:Err -> io:format("ERROR: ~p~p~p~n", [A, Err, erlang:get_stacktrace()])
  after
    Pbx:terminate()
  end.

run(#session{flow = Flow}, Ptr) when Ptr > length(Flow) -> finish;
run(State = #session{flow = Flow}, Ptr) ->
  Command = lists:nth(Ptr, Flow),
  {Action, NewState} = eval(Command, State),
  case Action of
    next ->
      run(NewState, Ptr + 1);
    {goto, N} ->
      run(NewState, N + 1);
    {exec, NewFlow} ->
      run(NewState#session{flow = NewFlow}, 1);
    finish -> finish
  end.

eval(stop, State) -> {finish, State};
eval([Command, Args], State) -> Command:run(Args, State);
eval(Command, State) -> Command:run(State).
