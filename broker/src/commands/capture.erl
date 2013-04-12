-module(capture).
-export([run/2]).
-include("session.hrl").

run(Args, #session{pbx = Pbx, js_context = JS, call_log = CallLog}) ->
  Min = proplists:get_value(min, Args),
  Max = proplists:get_value(max, Args),
  Timeout = proplists:get_value(timeout, Args, 5),
  FinishOnKey = proplists:get_value(finish_on_key, Args, "#"),

  ResourcePath = prepare_resource(Args, Pbx),
  mozjs:eval(JS, "digits = timeout = finish_key = null"),

  CallLog:info("Waiting user input", [{command, "capture"}, {action, "waiting"}]),

  case Pbx:capture(ResourcePath, Timeout, FinishOnKey, Min, Max) of
    finish_key -> mozjs:eval(JS, "finish_key = true");
    timeout -> mozjs:eval(JS, "timeout = true");
    short_entry -> mozjs:eval(JS, "finish_key = true");
    {digits, Digits} -> mozjs:eval(JS, "digits = '" ++ Digits ++ "'")
  end,
  next.

prepare_resource(Args, Pbx) ->
  parepare_localized_resource(Args, Pbx).

parepare_localized_resource(Args, Pbx) ->
  case proplists:get_value(resource, Args) of
    undefined -> prepare_url_resource(Args, Pbx);
    ResourceGuid -> resource:prepare(ResourceGuid, Pbx)
  end.

prepare_url_resource(Args, Pbx) ->
  case proplists:get_value(play, Args) of
    undefined -> throw(unknown_resource);
    Url -> resource:prepare_url_resource(Url, Pbx)
  end.