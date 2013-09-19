-module(flow).
-export([deserialize/1]).

deserialize(undefined) -> [];
deserialize(<<>>) -> [];
deserialize(CompFlow) ->
  Z = zlib:open(),
  zlib:inflateInit(Z),
  FlowYaml = iolist_to_binary(zlib:inflate(Z, binary_to_list(CompFlow))),
  zlib:close(Z),
  {ok, [Flow]} = yaml:load(FlowYaml, [{schema, yaml_schema_ruby}]),
  Flow.
