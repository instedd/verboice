-module(session).
-export([start_link/1, new/0, answer/2, answer/3, dial/4, stop/1]).

-behaviour(gen_fsm).
-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).
-export([ready/2, dialing/2]).

-define(SESSION(Id), {global, Id}).

-include("session.hrl").
-include("db.hrl").

start_link(SessionId) ->
  gen_fsm:start_link(?SESSION(SessionId), ?MODULE, SessionId, []).

new() ->
  SessionId = erlang:ref_to_list(make_ref()),
  SessionSpec = {SessionId, {session, start_link, [SessionId]}, temporary, 5000, worker, [session]},
  case supervisor:start_child(session_sup, SessionSpec) of
    {ok, _Pid} ->
      {ok, SessionId};
    Error = {error, _} ->
      Error
  end.

answer(SessionId, Pbx, ChannelId) ->
  gen_fsm:send_event(?SESSION(SessionId), {answer, Pbx, ChannelId}).

answer(SessionId, Pbx) ->
  io:format("answer(~p, ~p)~n", [SessionId, Pbx]),
  gen_fsm:send_event(?SESSION(SessionId), {answer, Pbx}).

dial(SessionId, RealBroker, Channel, QueuedCall) ->
  gen_fsm:send_event(?SESSION(SessionId), {dial, RealBroker, Channel, QueuedCall}).

stop(SessionId) ->
  gen_fsm:send_all_state_event(?SESSION(SessionId), stop).

%% @private

init(SessionId) ->
  {ok, ready, #session{session_id = SessionId}}.

ready({answer, Pbx, ChannelId}, Session) ->
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
  io:format("ANSWER!"),
  CallFlow = call_flow:find(Session#session.queued_call#queued_call.call_flow_id),
  NewSession = Session#session{pbx = Pbx, flow = CallFlow:commands()},
  spawn_monitor(fun() -> run(NewSession) end),

  {next_state, in_progress, NewSession};

dialing(timeout, Session) ->
  {stop, timeout, Session}.

handle_event(stop, _, Session) ->
  {stop, normal, Session}.

handle_sync_event(_Event, _From, StateName, Session) ->
  {reply, ok, StateName, Session}.

%% @private
handle_info({'DOWN', _Ref, process, _Pid, Reason}, _, State) ->
  io:format("DOWN!!!!!~n"),
  {stop, Reason, State};

handle_info(Info, _StateName, State) ->
  io:format("~p~n", [Info]),
  {noreply, State}.

%% @private
terminate(_Reason, _, #session{call_log = CallLog}) ->
  % call_log:update(CallLog#call_log{state = "completed", finished_at = calendar:universal_time()}),
  ok.

%% @private
code_change(_OldVsn, _StateName, State, _Extra) ->
  {ok, State}.

run(State = #session{pbx = Pbx}) ->
  JsContext = erjs_object:new(),
  RunState = State#session{js_context = JsContext},
  try run(RunState, 1) of
    _ -> ok
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
