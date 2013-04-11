-module(call_log).
-export([info/2]).
-define(TABLE_NAME, "call_logs").
-include("model.hrl").

info(Message, #call_log{id = CallId}) ->
  call_log_entry:create("info", CallId, Message).
