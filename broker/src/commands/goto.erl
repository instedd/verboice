-module(goto).
-export([run/2]).

run(N, _Pbx) ->
  {goto, N + 1}.