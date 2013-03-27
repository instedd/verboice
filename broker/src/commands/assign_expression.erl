-module(assign_expression).
-export([run/2]).
-include("session.hrl").

run(Args, #session{js_context = JS}) ->
  Name = proplists:get_value(name, Args),
  Data = proplists:get_value(data, Args),
  mozjs:eval(JS, binary_to_list(iolist_to_binary(io_lib:format("~s = ~s", [Name, Data])))),
  next.