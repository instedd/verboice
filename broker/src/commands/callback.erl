-module(callback).
-export([run/2]).
-include("session.hrl").
-include("db.hrl").

run(Args, Session = #session{session_id = SessionId, js_context = JS, call_log = CallLog, call_flow = CallFlow}) ->
  CallLog:info("Callback started", [{command, "callback"}, {action, "start"}]),
  Url = case proplists:get_value(url, Args) of
    undefined -> binary_to_list(CallFlow#call_flow.callback_url);
    X -> X
  end,
  Params = proplists:get_value(params, Args, []),
  QueryString = prepare_params(Params, "CallSid=" ++ SessionId, JS),

  Response = httpc:request(post, {Url, [], "application/x-www-form-urlencoded", QueryString}, [], []),

  {ok, {_StatusLine, _Headers, Body}} = Response,

  CallLog:trace(["Callback returned: ", Body], [{command, "callback"}, {action, "return"}]),

  Commands = twiml:parse(Body),
  io:format("~p~n", [Commands]),

  {{goto, 0}, Session#session{flow = Commands}}.

prepare_params([], QueryString, _JS) -> QueryString;
prepare_params([{Name, Expr} | Rest], QueryString, JS) ->
  Value = case erjs:eval(Expr, JS) of
    {Str, _} when is_list(Str) -> Str;
    {Num, _} when is_number(Num) -> integer_to_list(Num)
  end,
  prepare_params(Rest, QueryString ++ "&" ++ Name ++ "=" ++ Value, JS).
