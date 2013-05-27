-module(call_log_entry).
-export([create/4]).
-define(TABLE_NAME, "call_log_entries").
-include("model.hrl").

create(Severity, CallId, Message, Details) ->
  create(#call_log_entry{
    call_id = CallId,
    severity = Severity,
    details = serialize_details([{description, Message} | Details])
  }).

serialize_details(Details) -> iolist_to_binary(serialize_details(Details, "---\n")).

serialize_details([], Output) -> Output;
serialize_details([{Key, Value} | Rest], Output) ->
  FormattedValue = re:replace(Value, "'", "''", [global]),
  serialize_details(Rest, [Output | [":", atom_to_list(Key), ": ! '", FormattedValue, "'\n"]]).
