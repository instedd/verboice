-module(queued_call).
-export([reschedule/1]).
-define(TABLE_NAME, "queued_calls").

-define(MAP(QueuedCall),
  {ok, [Variables]} = yaml:load(QueuedCall#queued_call.variables, [{schema, yaml_schema_ruby}]),
  QueuedCall#queued_call{variables = Variables}
).

-include("model.hrl").

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