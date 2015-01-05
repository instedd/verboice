-module(callback).
-export([run/2]).
-include("session.hrl").
-include("db.hrl").
-include("uri.hrl").

run(Args, Session = #session{js_context = JS, call_log = CallLog, call_flow = CallFlow, callback_params = CallbackParams, project = Project}) ->
  Url = case proplists:get_value(url, Args) of
    undefined -> CallFlow#call_flow.callback_url;
    X -> list_to_binary(X)
  end,
  ResponseType = proplists:get_value(response_type, Args, flow),
  Params = proplists:get_value(params, Args, []),
  Variables = proplists:get_value(variables, Args, []),
  Method = util:to_string(proplists:get_value(method, Args, "post")),
  Async = proplists:get_value(async, Args),
  JsonBody = proplists:get_value(json_body, Args),
  TrustedApp = proplists:get_value(trusted_app, Args, false),

  QueryString = prepare_params(Params ++ Variables, [{"CallSid", CallLog:id()} | CallbackParams], JS),
  RequestUrl = interpolate(Url, Args, Session),

  PoirotMeta = [
    {url, iolist_to_binary(RequestUrl)},
    {method, iolist_to_binary(Method)},
    {body, {struct, util:to_poirot(QueryString)}}
  ],

  case Async of
    true ->
      poirot:new("Callback to " ++ RequestUrl, [{finalize, false}, {async, true}, {metadata, PoirotMeta}], fun() ->
        Task = [
          {url, RequestUrl},
          {method, Method},
          {body, QueryString},
          {activity, poirot:current_id()},
          {project_id, Project#project.id}
        ],
        delayed_job:enqueue(yaml:dump({map, Task, <<"!ruby/object:Jobs::CallbackJob">>}, [{schema, yaml_schema_ruby}]))
      end),
      {next, Session};

    _ ->
      poirot:new("Callback to " ++ RequestUrl, [{metadata, PoirotMeta}], fun() ->
        Uri = uri:parse(RequestUrl),

        UriOptions = case TrustedApp of
          true ->
            Account = account:find(Project#project.account_id),
            [{oauth2, guisso:get_trusted_token(Uri#uri.host, Account#account.email)}];
          _ -> []
        end,

        {ok, {_StatusLine, _Headers, Body}} = case Method of
          "get" ->
            (Uri#uri{query_string = QueryString}):get(UriOptions);
          _ ->
            case JsonBody of
              undefined -> Uri:post_form(QueryString, UriOptions);
              _ ->
                {PostBodyRef, _} = erjs:eval(JsonBody, JS),
                (Uri#uri{query_string = QueryString}):post("application/json", to_json(PostBodyRef, JS), UriOptions)
            end
        end,

        poirot:add_meta([{response, iolist_to_binary(Body)}]),
        handle_response(ResponseType, Body, Session)
      end)
  end.

to_json(Value, JS) ->
  {ok, Json} = json:encode(to_json_tuple(Value, JS)),
  Json.
to_json_tuple(Ref, JS) when is_reference(Ref) ->
  ObjList = erjs_context:get_object(Ref, JS),
  {lists:map(fun({K, V}) -> {K, to_json_tuple(V, JS)} end, ObjList)};
to_json_tuple(String, _JS) when is_list(String) ->
  list_to_binary(String);
to_json_tuple(Value, _) -> Value.

handle_response(flow, Body, Session) ->
  Commands = twiml:parse(Body),
  {{exec, Commands}, Session};

handle_response(variables, Body, Session) ->
  {ok, Response} = json:decode(Body),
  case Response of
    {Vars} -> {next, handle_response_variables(Vars, Session)};
    _ -> {next, Session}
  end;

handle_response(_, _, Session) -> {next, Session}.

handle_response_variables([], Session) -> Session;
handle_response_variables([{Name, Value} | Rest], Session = #session{js_context = JsContext}) ->
  VarName = binary_to_atom(<<"response_", Name/binary>>, utf8),
  VarValue = case Value of
    Bin when is_binary(Bin) -> binary_to_list(Bin);
    X -> X
  end,
  NewSession = Session#session{js_context = erjs_context:set(VarName, VarValue, JsContext)},
  handle_response_variables(Rest, NewSession).

prepare_params([], QueryString, _JS) -> QueryString;
prepare_params([{Name, Expr} | Rest], QueryString, JS) ->
  {Value, _} = erjs:eval(Expr, JS),
  NewQueryString = assign_from_js(QueryString, Name, Value, JS),
  prepare_params(Rest, NewQueryString, JS).

assign_from_js(QueryString, _, undefined, _) -> QueryString;
assign_from_js(QueryString, Prefix, Value, JS) ->
  if
    is_reference(Value) ->
      Properties = erjs_context:get_object(Value, JS),
      lists:foldl(fun({FName, FValue}, QS) ->
        assign_from_js(QS, [Prefix, "[", FName, "]"], FValue, JS)
      end, QueryString, Properties);
    true -> [{iolist_to_binary(Prefix), Value} | QueryString]
  end.

interpolate(Url, Args, #session{project = #project{id = ProjectId}, js_context = JsContext}) ->
  InterpolatedUrl = util:interpolate(Url, fun(VarNameBin) ->
    case proplists:get_value(external_service_guid, Args) of
      undefined ->
        {Value, _} = erjs:eval(VarNameBin, JsContext),
        list_to_binary(Value);
      SvcGuid ->
        Svc = external_service:find([{project_id, ProjectId}, {guid, SvcGuid}]),
        case Svc:global_variable_value_for(binary_to_list(VarNameBin)) of
          undefined -> <<>>;
          Value -> list_to_binary(Value)
        end
    end
  end),
  binary_to_list(InterpolatedUrl).
