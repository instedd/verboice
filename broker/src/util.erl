-module(util).
-export([to_string/1, binary_to_lower_atom/1, strip_nl/1]).

to_string(Value) when is_atom(Value) -> atom_to_list(Value);
to_string(Value) -> Value.

binary_to_lower_atom(Bin) -> list_to_atom(string:to_lower(binary_to_list(Bin))).

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
