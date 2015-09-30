-module(call_log_entry).
-export([create/4]).
-define(TABLE_NAME, "call_log_entries").
-define(MAP, [
  {details, marshal_zip_hash_serializer}
]).
-include_lib("erl_dbmodel/include/model.hrl").

create(Severity, CallId, Message, Details) ->
  create(#call_log_entry{
    call_id = CallId,
    severity = Severity,
    details = [{description, Message} | Details]
  }).
