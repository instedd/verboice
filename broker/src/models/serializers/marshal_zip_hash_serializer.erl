-module(marshal_zip_hash_serializer).
-export([load/1, dump/1]).

load(X) -> X.

dump(X) ->
  util:deflate(marshal:encode({hash, format_hash(X)})).

format_hash([]) -> [];
format_hash([{Key, Value} | T]) ->
  [{Key, {string, binary_to_list(iolist_to_binary(Value))}} | format_hash(T)].
