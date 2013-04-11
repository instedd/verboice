-module(call_log_entry).
-export([create/3]).
-define(TABLE_NAME, "call_log_entries").
-include("model.hrl").

create(Severity, CallId, Message) ->
  create(#call_log_entry{
    call_id = CallId,
    severity = Severity,
    details = serialize_details([{"description", Message}])
  }).

serialize_details(Details) -> serialize_details(Details, "---\n").

serialize_details([], Output) -> Output;
serialize_details([{Key, Value} | Rest], Output) ->
  serialize_details(Rest, [Output | [":", Key, ": ", Value, "\n"]]).