-module(util).
-export([md5hex/1, to_string/1, binary_to_lower_atom/1, strip_nl/1, binary_to_integer/1, parse_qs/1, normalize_phone_number/1, interpolate/2]).

md5hex(Data) ->
  Hash = crypto:hash(md5, Data),
  lists:flatten([io_lib:format("~2.16.0b", [B]) || <<B>> <= Hash]).

to_string(Value) when is_atom(Value) -> atom_to_list(Value);
to_string(Value) when is_binary(Value) -> binary_to_list(Value);
to_string(Value) when is_integer(Value) -> integer_to_list(Value);
to_string(Value) -> Value.

binary_to_lower_atom(Bin) -> list_to_atom(string:to_lower(binary_to_list(Bin))).

binary_to_integer(Bin) -> list_to_integer(binary_to_list(Bin)).

strip_nl(Binary) ->
  Size = size(Binary),
  NLSize = Size - 1,
  CRNLSize = Size - 2,
  case Binary of
    <<Bytes:CRNLSize/binary, "\r\n">> ->
      Bytes;
    <<Bytes:NLSize/binary, "\n">> ->
      Bytes
  end.

parse_qs(QS) ->
  parse_qs(QS, QS).

parse_qs([], OrigQS) ->
  httpd:parse_query(OrigQS);
parse_qs([$? | Rest], _) ->
  httpd:parse_query(Rest);
parse_qs([_ | Rest], OrigQS) ->
  parse_qs(Rest, OrigQS).

normalize_phone_number(Phone) ->
  iolist_to_binary(re:replace(Phone, "[\\+\\s-]", "", [global])).

interpolate(Text, Fun) -> interpolate(Text, Fun, <<>>).

interpolate(<<>>, _, Output) -> Output;
interpolate(Text, Fun, Output) ->
  case binary:split(Text, [<<${>>]) of
    [_] -> <<Output/binary, Text/binary>>;
    [H1, T1] ->
      case binary:split(T1, [<<$}>>]) of
        [_] -> <<Output/binary, Text/binary>>;
        [VarName, T2] ->
          Value = Fun(VarName),
          interpolate(T2, Fun, <<Output/binary, H1/binary, Value/binary>>)
      end
  end.
