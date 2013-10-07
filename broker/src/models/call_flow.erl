-module(call_flow).
-define(CACHE, true).
-define(TABLE_NAME, "call_flows").
-define(MAP(CallFlow), load_flow(CallFlow)).
-include_lib("erl_dbmodel/include/model.hrl").

load_flow(CallFlow = #call_flow{callback_url = CallbackUrl, broker_flow = CompFlow}) ->
  NewFlow = case CallbackUrl of
    undefined -> flow:deserialize(CompFlow);
    _ -> [answer, [callback, [{url, binary_to_list(CallbackUrl)}]]]
  end,
  Config = case CallFlow#call_flow.encrypted_config of
    undefined -> [];
    CryptConfig ->
      {ok, Secret} = application:get_env(verboice, crypt_secret),
      [PlainConfig] = marshal:decode(aes:decrypt(Secret, base64:decode(CryptConfig))),
      PlainConfig
  end,
  CallFlow#call_flow{broker_flow = NewFlow, encrypted_config = Config}.
