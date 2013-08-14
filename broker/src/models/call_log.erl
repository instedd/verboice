-module(call_log).
-export([error/3, info/3, trace/3]).
-define(TABLE_NAME, "call_logs").
-include_lib("erl_dbmodel/include/model.hrl").

error(Message, Details, #call_log{id = CallId}) ->
  call_log_entry:create("error", CallId, Message, Details).

info(Message, Details, #call_log{id = CallId}) ->
  call_log_entry:create("info", CallId, Message, Details).

trace(Message, Details, #call_log{id = CallId}) ->
  call_log_entry:create("trace", CallId, Message, Details).

