-module(queued_call).
-export([reschedule/1, start_session/2]).
-define(TABLE_NAME, "queued_calls").
-include("session.hrl").
-define(MAP(CallFlow), load_flow(CallFlow)).
-include_lib("erl_dbmodel/include/model.hrl").

load_flow(QueuedCall = #queued_call{flow = CompFlow}) ->
  QueuedCall#queued_call{flow = flow:deserialize(CompFlow)}.

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

start_session(Session, #queued_call{call_flow_id = CallFlowId}) when is_number(CallFlowId) ->
  CallFlow = call_flow:find(CallFlowId),
  Session#session{flow = CallFlow#call_flow.flow, call_flow = CallFlow};
start_session(Session, #queued_call{callback_url = CallbackUrl}) when is_binary(CallbackUrl) ->
  Session#session{flow = [answer, [callback, [{url, binary_to_list(CallbackUrl)}]]]};
start_session(Session, #queued_call{flow = Flow}) ->
  Session#session{flow = Flow}.
