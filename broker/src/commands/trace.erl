-module(trace).
-export([run/2]).
-include("session.hrl").

run(Args, Session = #session{js_context = JS}) ->
  Expression = proplists:get_value(expression, Args),
  {_Result, _} = erjs:eval(Expression, JS),
  % io:format("~s~n", [Result]),
  {next, Session}.