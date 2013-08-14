-module(call_flow).
-define(CACHE, true).
-define(TABLE_NAME, "call_flows").

-define(MAP(CallFlow), load_flow(CallFlow)).

-include_lib("erl_dbmodel/include/model.hrl").

load_flow(CallFlow = #call_flow{callback_url = CallbackUrl, flow = CompFlow}) ->
  NewFlow = case CallbackUrl of
    undefined ->
      Z = zlib:open(),
      zlib:inflateInit(Z),
      FlowYaml = iolist_to_binary(zlib:inflate(Z, binary_to_list(CompFlow))),
      zlib:close(Z),
      {ok, [Flow]} = yaml:load(FlowYaml, [{schema, yaml_schema_ruby}]),
      Flow;
    _ -> [answer, [callback, [{url, binary_to_list(CallbackUrl)}]]]
  end,
  CallFlow#call_flow{flow = NewFlow}.
