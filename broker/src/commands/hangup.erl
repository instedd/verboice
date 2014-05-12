-module(hangup).
-export([run/1]).
-include("session.hrl").

run(Session = #session{pbx = Pbx}) ->
  poirot:log(info, "Hangup"),
  Pbx:hangup(),
  {next, Session}.
