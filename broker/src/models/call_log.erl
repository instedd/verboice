-module(call_log).
-export([info/3, trace/3]).
-define(TABLE_NAME, "call_logs").
-include("model.hrl").

info(Message, Details, #call_log{id = CallId}) ->
  call_log_entry:create("info", CallId, Message, Details).

trace(Message, Details, #call_log{id = CallId}) ->
  call_log_entry:create("trace", CallId, Message, Details).
