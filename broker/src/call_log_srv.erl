-module(call_log_srv).
-export([new/2, error/3, info/3, trace/3, trace_record/5, update/2, id/1, associate_pbx_log/2, hangup/2]).

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-record(state, {call_log, owner_pid, timeout}).

-define(STOP_TIMEOUT, 500).

-include("db.hrl").

new(SessionId, CallLog) ->
  {ok, Pid} = gen_server:start({global, {call_log_srv, SessionId}}, ?MODULE, {CallLog, self()}, []),
  {?MODULE, Pid}.

error(Message, Details, {?MODULE, Pid}) ->
  gen_server:cast(Pid, {log, error, Message, Details}).

info(Message, Details, {?MODULE, Pid}) ->
  gen_server:cast(Pid, {log, info, Message, Details}).

trace(Message, Details, {?MODULE, Pid}) ->
  gen_server:cast(Pid, {log, trace, Message, Details}).

trace_record(CallFlowId, StepId, StepName, Result, {?MODULE, Pid}) ->
  gen_server:cast(Pid, {trace_record, CallFlowId, StepId, StepName, Result}).

update(Fields, {?MODULE, Pid}) ->
  gen_server:cast(Pid, {update, Fields}).

associate_pbx_log(SessionId, PbxLogId) ->
  gen_server:cast({global, {call_log_srv, SessionId}}, {associate_pbx_log, PbxLogId}).

hangup(SessionId, Reason) ->
  gen_server:cast({global, {call_log_srv, SessionId}}, {hangup, Reason}).

id({?MODULE, Pid}) ->
  gen_server:call(Pid, get_id).

%% @private
init({CallLog, Owner}) ->
  monitor(process, Owner),
  case CallLog#call_log.id of
    undefined -> gen_server:cast(self(), create);
    _ -> ok
  end,
  {ok, #state{call_log = CallLog, owner_pid = Owner, timeout = infinity}}.

%% @private
handle_call(get_id, _From, State = #state{call_log = CallLog, timeout = Timeout}) ->
  {reply, CallLog#call_log.id, State, Timeout}.

%% @private
handle_cast(create, State = #state{call_log = CallLog, timeout = Timeout}) ->
  NewCallLog = CallLog:create(),
  {noreply, State#state{call_log = NewCallLog}, Timeout};

handle_cast({log, Level, Message, Details}, State = #state{call_log = CallLog, timeout = Timeout}) ->
  % CallLog:Level(Message, Details),
  call_log_entry_srv:log(CallLog#call_log.id, Level, Message, Details),
  {noreply, State, Timeout};

handle_cast({update, Fields}, State = #state{call_log = CallLog, timeout = Timeout}) ->
  NewCallLog = CallLog:update(Fields),
  {noreply, State#state{call_log = NewCallLog}, Timeout};

handle_cast({trace_record, CallFlowId, StepId, StepName, Result}, State = #state{call_log = CallLog, timeout = Timeout}) ->
  % TraceRecord = #trace_record{
  %   call_flow_id = CallFlowId,
  %   step_id = StepId,
  %   step_name = StepName,
  %   call_id = CallLog#call_log.id,
  %   result = Result
  % },
  % TraceRecord:save(),
  call_log_entry_srv:trace(CallLog#call_log.id, CallFlowId, StepId, StepName, Result),
  {noreply, State, Timeout};

handle_cast({associate_pbx_log, PbxLogId}, State = #state{call_log = CallLog, timeout = Timeout}) ->
  NewCallLog = call_log:update(CallLog#call_log{pbx_logs_guid = PbxLogId}),
  {noreply, State#state{call_log = NewCallLog}, Timeout};

handle_cast({hangup, {_Code, <<"Unknown">>}}, State = #state{timeout = Timeout}) ->
  {noreply, State, Timeout};

handle_cast({hangup, {Code, Reason}}, State = #state{call_log = CallLog, timeout = Timeout}) ->
  FullCode = case Code of
    undefined -> undefined;
    <<"0">> -> undefined;
    _ -> "ISDN:" ++ binary_to_list(Code)
  end,
  NewCallLog = call_log:update(CallLog#call_log{fail_code = FullCode, fail_details = Reason}),
  {noreply, State#state{call_log = NewCallLog}, Timeout}.

%% @private
handle_info({'DOWN', _Ref, process, Pid, _Reason}, State = #state{owner_pid = Pid}) ->
  % Wait for STOP_TIMEOUT milliseconds before actually stopping, to receive any pending updates (see issue 708)
  {noreply, State#state{timeout = ?STOP_TIMEOUT}, ?STOP_TIMEOUT};

handle_info(timeout, State) ->
  {stop, normal, State};

handle_info(_Info, State) ->
  {noreply, State}.

%% @private
terminate(_Reason, _State) ->
  ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
