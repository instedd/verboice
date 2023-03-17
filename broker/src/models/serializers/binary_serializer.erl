-module(binary_serializer).
-export([load/1, dump/1]).

load(null) -> undefined;
load(undefined) -> undefined;
load(Binary) ->
  Z = zlib:open(),
  zlib:inflateInit(Z),
  Term = binary_to_term(iolist_to_binary(zlib:inflate(Z, binary_to_list(Binary)))),
  zlib:close(Z),
  Term.

dump(null) -> null;
dump(undefined) -> null;
dump(Term) ->
  Z = zlib:open(),
  zlib:deflateInit(Z),
  CompTerm = iolist_to_binary(zlib:deflate(Z, term_to_binary(Term), full)),
  zlib:close(Z),
  CompTerm.
