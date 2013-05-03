-module(capture).
-export([run/2]).
-include("session.hrl").

run(Args, Session = #session{pbx = Pbx, js_context = JS, call_log = CallLog}) ->
  Min = proplists:get_value(min, Args),
  Max = proplists:get_value(max, Args),
  Timeout = proplists:get_value(timeout, Args, 5),
  FinishOnKey = proplists:get_value(finish_on_key, Args, "#"),

  Caption = prepare_caption(Args, Session),
  {_, JS2} = erjs:eval("digits = timeout = finish_key = null", JS),

  CallLog:info("Waiting user input", [{command, "capture"}, {action, "waiting"}]),

  JS3 = case Pbx:capture(Caption, Timeout, FinishOnKey, Min, Max) of
    finish_key -> erjs_object:set(finish_key, true, JS2);
    timeout -> erjs_object:set(timeout, true, JS2);
    short_entry -> erjs_object:set(finish_key, true, JS2);
    {digits, Digits} -> erjs_object:set(digits, Digits, JS2)
  end,
  {next, Session#session{js_context = JS3}}.

prepare_caption(Args, Session) ->
  parepare_localized_resource(Args, Session).

parepare_localized_resource(Args, Session) ->
  case proplists:get_value(resource, Args) of
    undefined -> prepare_url_resource(Args, Session);
    ResourceGuid -> resource:prepare(ResourceGuid, Session)
  end.

prepare_url_resource(Args, Session) ->
  case proplists:get_value(play, Args) of
    undefined -> throw(unknown_resource);
    Url -> resource:prepare_url_resource(Url, Session)
  end.