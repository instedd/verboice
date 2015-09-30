-module(capture).
-export([run/2]).
-include("session.hrl").

run(Args, Session = #session{pbx = Pbx, js_context = JS}) ->
  Min = proplists:get_value(min, Args),
  Max = proplists:get_value(max, Args),
  Timeout = proplists:get_value(timeout, Args, 5),
  FinishOnKey = proplists:get_value(finish_on_key, Args, "#"),

  Caption = prepare_caption(Args, Session),
  {_, JS2} = erjs:eval("digits = timeout = finish_key = null", JS),

  poirot:log(info, "Waiting user input (timeout: ~p, min: ~p, max: ~p, finish: ~s)", [Timeout, Min, Max, FinishOnKey]),

  JS3 = case Pbx:capture(Caption, Timeout, FinishOnKey, Min, Max) of
    finish_key ->
      poirot:log(info, "User pressed the finish key"),
      erjs_context:set(finish_key, true, JS2);
    timeout ->
      poirot:log(info, "User timeout"),
      erjs_context:set(timeout, true, JS2);
    short_entry ->
      poirot:log(info, "User didn't press enough digits"),
      erjs_context:set(finish_key, true, JS2);
    {digits, Digits} ->
      poirot:log(info, "User pressed: ~s", [Digits]),
      erjs_context:set(digits, Digits, JS2)
  end,
  {next, Session#session{js_context = JS3}}.

prepare_caption(Args, Session) ->
  parepare_localized_resource(Args, Session).

parepare_localized_resource(Args, Session) ->
  case proplists:get_value(resource, Args) of
    undefined -> prepare_url_resource(Args, Session);
    ResourceGuid ->
      case proplists:get_value(language, Args) of
        undefined -> resource:prepare(ResourceGuid, Session);
        Language -> resource:prepare(ResourceGuid, Session, Language)
      end
  end.

prepare_url_resource(Args, Session) ->
  case proplists:get_value(play, Args) of
    undefined -> prepare_text_resource(Args, Session);
    Url -> resource:prepare_url_resource(Url, Session)
  end.

prepare_text_resource(Args, Session) ->
  case proplists:get_value(say, Args) of
    undefined -> throw(unknown_resource);
    Text -> resource:prepare_text_resource(list_to_binary(Text), Session)
  end.
