-module(if_command).
-export([run/2]).
-include("session.hrl").

run(Args, #session{js_context = JS}) ->
  Condition = proplists:get_value(condition, Args),
  Then = proplists:get_value(then, Args),
  Else = proplists:get_value(else, Args),
  case mozjs:eval(JS, Condition) of
    true -> run_branch(Then);
    _    -> run_branch(Else)
  end.

run_branch(undefined) -> next;
run_branch(Branch) -> {goto, Branch}.