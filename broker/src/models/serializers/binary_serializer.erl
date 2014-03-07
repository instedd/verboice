-module(binary_serializer).
-export([load/1, dump/1]).

load(Binary) ->
  binary_to_term(Binary).

dump(Term) ->
  term_to_binary(Term).
