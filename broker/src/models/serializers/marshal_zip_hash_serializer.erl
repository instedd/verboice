-module(marshal_zip_hash_serializer).
-export([load/1, dump/1]).

load(X) -> X.

dump(X) ->
  util:deflate(marshal:encode({hash, X})).
