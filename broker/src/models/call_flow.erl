-module(call_flow).
-export([flow/1]).
-define(CACHE, true).
-define(TABLE_NAME, "call_flows").
-define(MAP, [
  {broker_flow, flow_serializer},
  {encrypted_config, encrypted_config_serializer}
]).
-include_lib("erl_dbmodel/include/model.hrl").

flow(#call_flow{callback_url = undefined, broker_flow = Flow}) ->
  Flow;
flow(#call_flow{callback_url = CallbackUrl}) ->
  flow:callback_flow(CallbackUrl).
