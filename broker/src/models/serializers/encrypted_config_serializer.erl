-module(encrypted_config_serializer).
-export([load/1, dump/1]).

load(Config) ->
  case Config of
    undefined -> [];
    CryptConfig ->
      {ok, Secret} = application:get_env(verboice, crypt_secret),
      [PlainConfig] = marshal:decode(aes:decrypt(Secret, base64:decode(CryptConfig))),
      PlainConfig
  end.

dump([]) -> undefined;
dump(undefined) -> undefined;
dump(_) -> throw(not_implemented).
