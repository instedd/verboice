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
  spawn_run(NewSession),

  {next_state, in_progress, NewSession}.

ready({dial, RealBroker, Channel, QueuedCall}, _From, Session) ->
  error_logger:info_msg("Session (~p) dial", [Session#session.session_id]),
  CallLog = call_log:find(QueuedCall#queued_call.call_log_id),

  NewSession = Session#session{
    channel = Channel,
    address = QueuedCall#queued_call.address,
    call_log = call_log:update(CallLog#call_log{state = "active"}),
    queued_call = QueuedCall
  },

  case RealBroker:dispatch(NewSession) of
    {error, _} -> {stop, normal, unavailable, Session};
    _ ->
      CallLog:info(["Dialing to ", QueuedCall#queued_call.address, " through channel ", Channel#channel.name], []),
      {reply, ok, dialing, NewSession}
  end.

dialing({answer, Pbx}, Session) ->
  error_logger:info_msg("Session (~p) answer", [Session#session.session_id]),
  CallFlow = call_flow:find(Session#session.queued_call#queued_call.call_flow_id),
  NewSession = Session#session{pbx = Pbx, flow = CallFlow:commands()},
  spawn_run(NewSession),

  {next_state, in_progress, NewSession};

dialing({reject, Reason}, Session = #session{session_id = SessionId, call_log = CallLog}) ->
  error_logger:info_msg("Session (~p) rejected, reason: ~p", [SessionId, Reason]),
  CallLog:error(["Call was rejected. (Reason: ", atom_to_list(Reason),")"], []),
  {stop, normal, Session};

dialing(timeout, Session) ->
  {stop, timeout, Session}.

in_progress({completed, ok}, Session = #session{call_log = CallLog}) ->
  NewCallLog = call_log:update(CallLog#call_log{state = "completed", finished_at = calendar:universal_time()}),
  {stop, normal, Session#session{call_log = NewCallLog}};

in_progress({completed, {error, Reason}}, Session = #session{call_log = CallLog}) ->
  NewCallLog = call_log:update(CallLog#call_log{state = "failed", fail_reason = Reason, finished_at = calendar:universal_time()}),
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

spawn_run(State) ->
  SessionPid = self(),
  spawn_monitor(fun() ->
    {Result, _NewState} = run(State),
    gen_fsm:send_event(SessionPid, {completed, Result})
  end).

run(State = #session{pbx = Pbx}) ->
  JsContext = erjs_object:new(),
  RunState = State#session{js_context = JsContext},
  try run(RunState, 1) of
    X -> X
  after
    Pbx:terminate()
  end.

run(State = #session{flow = Flow}, Ptr) when Ptr > length(Flow) -> {ok, State};
run(State = #session{flow = Flow, call_log = CallLog}, Ptr) ->
  Command = lists:nth(Ptr, Flow),
  try eval(Command, State) of
    {Action, NewState} ->
      case Action of
        next ->
          run(NewState, Ptr + 1);
        {goto, N} ->
          run(NewState, N + 1);
        {exec, NewFlow} ->
          run(NewState#session{flow = NewFlow}, 1);
        finish ->
          {ok, NewState}
      end
  catch
    hangup ->
      CallLog:info("The user hang up", []),
      {{error, hangup}, State};
    Class:Error ->
      CallLog:error(["Error ", io_lib:format("~p:~p", [Class, Error])], []),
      error_logger:error_msg("Error during session ~p, call log ~p: ~p:~p~n~p~n",
        [State#session.session_id, CallLog#call_log.id, Class, Error, erlang:get_stacktrace()]),
      {{error, error}, State}
  end.

eval(stop, State) -> {finish, State};
eval([Command, Args], State) -> Command:run(Args, State);
eval(Command, State) -> Command:run(State).
