-module(while).
-export([run/2]).
-include("session.hrl").

run(Args, #session{js_context = JS}) ->
  Condition = proplists:get_value(condition, Args),
  case mozjs:eval(JS, Condition) of
    true ->
      Block = proplists:get_value(block, Args),
      {goto, Block};
    _ ->
      next
  end.
