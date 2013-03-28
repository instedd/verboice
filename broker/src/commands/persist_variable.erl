-module(persist_variable).
-export([run/2]).
-include("session.hrl").

run(Args, #session{js_context = JS}) ->
  Name = proplists:get_value(name, Args),
  Expression = proplists:get_value(expression, Args),
  _Value = mozjs:eval(JS, binary_to_list(iolist_to_binary(io_lib:format("var_~s = ~s", [Name, Expression])))),
  next.