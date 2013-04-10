-module(call_flow).
-export([get_commands/1]).

get_commands(Id) ->
  [CallbackUrl, CompFlow] = db:select_one("SELECT callback_url, flow FROM call_flows WHERE id = ~p", [Id]),
  case CallbackUrl of
    undefined ->
      Z = zlib:open(),
      zlib:inflateInit(Z),
      FlowYaml = iolist_to_binary(zlib:inflate(Z, binary_to_list(CompFlow))),
      {ok, [Flow]} = yaml:load(FlowYaml, [{schema, yaml_schema_ruby}]),
      Flow;
    _ -> [answer, [callback, [{url, binary_to_list(CallbackUrl)}]]]
  end.
