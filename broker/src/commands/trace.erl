-module(trace).
-export([run/2]).
-include("session.hrl").

run(Args, #session{js_context = JS}) ->
  Expression = proplists:get_value(expression, Args),
  Result = mozjs:eval(JS, Expression),
  io:format("~s~n", [Result]),
  next.