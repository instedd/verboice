-module(flow).
-export([deserialize/1, serialize/1, callback_flow/1]).

deserialize(undefined) -> [];
deserialize(<<>>) -> [];
deserialize(CompFlow) ->
  Z = zlib:open(),
  zlib:inflateInit(Z),
  FlowYaml = iolist_to_binary(zlib:inflate(Z, binary_to_list(CompFlow))),
  zlib:close(Z),
  {ok, [Flow]} = yaml:load(FlowYaml, [{schema, yaml_schema_ruby}]),
  Flow.

serialize([]) -> undefined;
serialize(Flow) ->
  FlowYaml = yaml:dump(Flow, [{schema, yaml_schema_ruby}]),
  Z = zlib:open(),
  zlib:deflateInit(Z),
  CompFlow = iolist_to_binary(zlib:deflate(Z, FlowYaml, full)),
  zlib:close(Z),
  CompFlow.

callback_flow(CallbackUrl) ->
  [answer, [callback, [{url, binary_to_list(CallbackUrl)}]]].
