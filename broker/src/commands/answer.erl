-module(answer).
-export([run/1]).
-include("session.hrl").

run(Session = #session{pbx = Pbx}) ->
  poirot:log(info, "Answer"),
  Pbx:answer(),
  {next, Session}.
