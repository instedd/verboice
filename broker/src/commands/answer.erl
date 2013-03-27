-module(answer).
-export([run/1]).
-include("session.hrl").

run(#session{pbx = Pbx}) ->
  Pbx:answer(),
  next.