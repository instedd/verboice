-module(capture).
-export([run/2]).
-include("session.hrl").

run(Args, #session{pbx = Pbx, js_context = JS}) ->
  Min = proplists:get_value(min, Args),
  Max = proplists:get_value(max, Args),
  Timeout = proplists:get_value(timeout, Args),
  [FinishOnKey] = proplists:get_value(finish_on_key, Args),
  ResourceGuid = proplists:get_value(resource, Args),

  ResourcePath = resource:prepare(ResourceGuid, Pbx),
  mozjs:eval(JS, "digits = timeout = finish_key = null"),

  case Pbx:capture(ResourcePath, Timeout, FinishOnKey, Min, Max) of
    finish_key -> mozjs:eval(JS, "finish_key = true");
    timeout -> mozjs:eval(JS, "timeout = true");
    short_entry -> mozjs:eval(JS, "finish_key = true");
    {digits, Digits} -> mozjs:eval(JS, "digits = '" ++ Digits ++ "'")
  end,
  next.