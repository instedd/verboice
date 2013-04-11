-module(call_flow).
-export([commands/1]).

-define(TABLE_NAME, "call_flows").
-include("model.hrl").

commands(#call_flow{callback_url = CallbackUrl, flow = CompFlow}) ->
  case CallbackUrl of
    undefined ->
      Z = zlib:open(),
      zlib:inflateInit(Z),
      FlowYaml = iolist_to_binary(zlib:inflate(Z, binary_to_list(CompFlow))),
      {ok, [Flow]} = yaml:load(FlowYaml, [{schema, yaml_schema_ruby}]),
      Flow;
    _ -> [answer, [callback, [{url, binary_to_list(CallbackUrl)}]]]
  end.
