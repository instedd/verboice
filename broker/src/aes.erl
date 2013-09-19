-module(aes).
-export([decrypt/2, generate_key_and_iv/1]).

decrypt(Password, Data) ->
  {Key, IV} = generate_key_and_iv(Password),
  Result = crypto:block_decrypt(aes_cbc256, Key, IV, Data),
  remove_padding(Result).

generate_key_and_iv(Password) ->
  generate_key_and_iv(to_binary(Password), <<>>, <<>>).

generate_key_and_iv(_Data, _, <<Key:32/binary, IV:16/binary, _/binary>>) -> {Key, IV};
generate_key_and_iv(Data, PrevD, ConcatD) ->
  NextD = d(Data, PrevD),
  generate_key_and_iv(Data, NextD, <<ConcatD/binary, NextD/binary>>).

d(Data, PrevD) ->
  hash(<<PrevD/binary, Data/binary>>).

hash(Data) -> hash(Data, 2048).
hash(Data, 0) -> Data;
hash(Data, N) -> hash(crypto:hash(md5, Data), N - 1).

to_binary(X) when is_list(X) -> list_to_binary(X);
to_binary(X) when is_binary(X) -> X.

remove_padding(X) ->
  PadLength = binary:last(X),
  binary:part(X, 0, size(X) - PadLength).
