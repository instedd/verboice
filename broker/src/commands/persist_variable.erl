-module(persist_variable).
-export([run/2]).
-include("session.hrl").

run(Args, Session = #session{js_context = JS}) ->
  Name = proplists:get_value(name, Args),
  Expression = proplists:get_value(expression, Args),
  {_, JS2} = erjs:eval(iolist_to_binary(["var_", Name, " = ", Expression]), JS),
  {next, Session#session{js_context = JS2}}.