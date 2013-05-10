-module(callback).
-export([run/2]).
-include("session.hrl").
-include("db.hrl").

run(Args, Session = #session{session_id = SessionId, js_context = JS, call_log = CallLog, call_flow = CallFlow}) ->
  CallLog:info("Callback started", [{command, "callback"}, {action, "start"}]),
  Url = case proplists:get_value(url, Args) of
    undefined -> CallFlow#call_flow.callback_url;
    X -> list_to_binary(X)
  end,
  ResponseType = proplists:get_value(response_type, Args, flow),
  Params = proplists:get_value(params, Args, []),
  Variables = proplists:get_value(variables, Args, []),
  Method = proplists:get_value(method, Args, "post"),

  QueryString = prepare_params(Params ++ Variables, "CallSid=" ++ SessionId, JS),
  RequestUrl = interpolate(Url, Args, Session),
  {ok, {_StatusLine, _Headers, Body}} = case Method of
    "get" ->
      httpc:request(RequestUrl ++ "?" ++ QueryString);
    _ ->
      httpc:request(post, {RequestUrl, [], "application/x-www-form-urlencoded", QueryString}, [], [])
  end,

  CallLog:trace(["Callback returned: ", Body], [{command, "callback"}, {action, "return"}]),
  handle_response(ResponseType, Body, Session).

handle_response(flow, Body, Session) ->
  Commands = twiml:parse(Body),
  io:format("~p~n", [Commands]),
  {{goto, 0}, Session#session{flow = Commands}};

handle_response(variables, Body, Session) ->
  {ok, Response} = json:decode(Body),
  case Response of
    {Vars} -> {next, handle_response_variables(Vars, Session)};
    _ -> {next, Session}
  end.

handle_response_variables([], Session) -> Session;
handle_response_variables([{Name, Value} | Rest], Session = #session{js_context = JsContext}) ->
  VarName = binary_to_atom(<<"response_", Name/binary>>, utf8),
  VarValue = case Value of
    Bin when is_binary(Bin) -> binary_to_list(Bin);
    X -> X
  end,
  NewSession = Session#session{js_context = erjs_object:set(VarName, VarValue, JsContext)},
  handle_response_variables(Rest, NewSession).

prepare_params([], QueryString, _JS) -> QueryString;
prepare_params([{Name, Expr} | Rest], QueryString, JS) ->
  Value = case erjs:eval(Expr, JS) of
    {Str, _} when is_list(Str) -> Str;
    {Num, _} when is_number(Num) -> integer_to_list(Num)
  end,
  prepare_params(Rest, QueryString ++ "&" ++ Name ++ "=" ++ Value, JS).

interpolate(Url, Args, _Session) ->
  InterpolatedUrl = util:interpolate(Url, fun(VarNameBin) ->
    case proplists:get_value(external_service_guid, Args) of
      undefined -> <<>>;
      SvcGuid ->
        Svc = external_service:find({guid, SvcGuid}),
        case Svc:global_variable_value_for(binary_to_list(VarNameBin)) of
          undefined -> <<>>;
          Value -> list_to_binary(Value)
        end
    end
  end),
  binary_to_list(InterpolatedUrl).
