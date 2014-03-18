-module(poirot_gen_transform).

-export([parse_transform/2]).

parse_transform(AST, _Options) ->
  parse_trans:plain_transform(fun do_transform/1, AST).

do_transform({atom, L, gen_server}) ->
  {atom, L, poirot_gen_server};

do_transform(_) ->
  continue.
