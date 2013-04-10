-module(callback).
-export([run/2]).
-include("session.hrl").

run(Args, #session{session_id = SessionId, js_context = JS}) ->
  Url = proplists:get_value(url, Args, "http://localhost:4567/"),
  Params = proplists:get_value(params, Args, []),
  QueryString = prepare_params(Params, "CallSid=" ++ erlang:ref_to_list(SessionId), JS),

  Response = httpc:request(post, {Url, [], "application/x-www-form-urlencoded", QueryString}, [], []),

  {ok, {_StatusLine, _Headers, Body}} = Response,

  Commands = twiml:parse(Body),
  io:format("~p~n", [Commands]),

  {exec, Commands}.

prepare_params([], QueryString, _JS) -> QueryString;
prepare_params([{Name, Expr} | Rest], QueryString, JS) ->
  Value = case mozjs:eval(JS, Expr) of
    Str when is_list(Str) -> Str;
    Num when is_number(Num) -> integer_to_list(Num)
  end,
  prepare_params(Rest, QueryString ++ "&" ++ Name ++ "=" ++ Value, JS).
