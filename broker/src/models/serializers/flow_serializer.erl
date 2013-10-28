-module(flow_serializer).
-export([load/1, dump/1]).

load(Flow) -> flow:deserialize(Flow).

dump(Flow) -> flow:serialize(Flow).
