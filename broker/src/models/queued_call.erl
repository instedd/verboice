-module(queued_call).
-export([reschedule/1, start_session/1]).
-define(TABLE_NAME, "queued_calls").
-include("session.hrl").
-define(MAP(QueuedCall), load_queued_call(QueuedCall)).
-include_lib("erl_dbmodel/include/model.hrl").

load_queued_call(QueuedCall = #queued_call{flow = CompFlow}) ->
  CallbackParams = util:safe_load_yaml(QueuedCall#queued_call.callback_params),
  Variables = util:safe_load_yaml(QueuedCall#queued_call.variables),
  QueuedCall#queued_call{flow = flow:deserialize(CompFlow), callback_params = CallbackParams, variables = Variables}.

reschedule(#queued_call{schedule_id = undefined}) -> no_schedule;
reschedule(QueuedCall = #queued_call{schedule_id = ScheduleId}) ->
  Schedule = schedule:find(ScheduleId),
  reschedule(QueuedCall, Schedule).

reschedule(_, #schedule{retries = undefined}) -> max_retries;
reschedule(Q, S) when Q#queued_call.retries >= length(S#schedule.retries) -> max_retries;
reschedule(Q = #queued_call{retries = Retries, time_zone = TimeZone}, S) ->
  NextRetryOffset = trunc(lists:nth(Retries + 1, S#schedule.retries) * 60 * 60),
  TimeZoneOffset = tz_server:get_timezone_offset(TimeZone),
  NextRetry = calendar:datetime_to_gregorian_seconds(calendar:universal_time()) + NextRetryOffset + TimeZoneOffset,
  RetryTime = calendar:gregorian_seconds_to_datetime(S:next_available_time(NextRetry) - TimeZoneOffset),
  queued_call:create(Q#queued_call{not_before = RetryTime, retries = Retries + 1}).

start_session(QueuedCall = #queued_call{call_flow_id = CallFlowId}) when is_number(CallFlowId) ->
  CallFlow = call_flow:find(CallFlowId),
  start_session(#session{flow = CallFlow#call_flow.broker_flow, call_flow = CallFlow}, QueuedCall);
start_session(QueuedCall = #queued_call{callback_url = CallbackUrl}) when is_binary(CallbackUrl) ->
  start_session(#session{flow = [answer, [callback, [{url, binary_to_list(CallbackUrl)}]]]}, QueuedCall);
start_session(QueuedCall = #queued_call{flow = Flow}) ->
  start_session(#session{flow = Flow}, QueuedCall).

start_session(Session, QueuedCall) ->
  Project = project:find(QueuedCall#queued_call.project_id),
  {StatusUrl, StatusUser, StatusPass} = case QueuedCall#queued_call.status_callback_url of
    undefined -> project:status_callback(Project);
    <<>> -> project:status_callback(Project);
    Url -> {Url, undefined, undefined}
  end,
  Session#session{
    address = QueuedCall#queued_call.address,
    status_callback_url = StatusUrl,
    status_callback_user = StatusUser,
    status_callback_password = StatusPass,
    callback_params = QueuedCall#queued_call.callback_params,
    queued_call = QueuedCall,
    project = Project
  }.
