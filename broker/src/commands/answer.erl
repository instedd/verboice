-module(answer).
-export([run/1]).

run(Pbx) ->
  Pbx:answer(),
  next.