-module(call_log_entry).
-export([create/4]).
-define(TABLE_NAME, "call_log_entries").
-include_lib("erl_dbmodel/include/model.hrl").

create(Severity, CallId, Message, Details) ->
  create(#call_log_entry{
    call_id = CallId,
    severity = Severity,
    details = serialize_details([{description, Message} | Details])
  }).

serialize_details(Details) ->
  case json:encode({format_details(Details)}) of
    {ok, Json} -> Json;
    _ -> undefined
  end.

format_details([]) -> [];
format_details([{Key, Value} | T]) ->
  [{Key, iolist_to_binary(Value)} | format_details(T)].
