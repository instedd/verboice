-module(contact_scheduled_call).
-define(TABLE_NAME, "contact_scheduled_calls").
-export([record_last_call/1]).
-include_lib("erl_dbmodel/include/model.hrl").

record_last_call(Call) ->
  ContactId = Call#queued_call.contact_id,
  ScheduledCallId = Call#queued_call.scheduled_call_id,
  if
    ContactId /= undefined andalso ScheduledCallId /= undefined ->
      ContactScheduledCall = find_or_create([{contact_id, ContactId}, {scheduled_call_id, ScheduledCallId}]),
      ContactScheduledCall:update([{last_called_at, calendar:universal_time()}]);
    true -> ok
  end.
