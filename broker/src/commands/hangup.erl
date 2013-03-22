-module(hangup).
-export([run/1]).

run(Pbx) ->
  Pbx:hangup(),
  next.