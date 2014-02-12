-module(hangup).
-export([run/1]).
-include("session.hrl").

run(Session = #session{pbx = Pbx}) ->
  lager:info("Hangup"),
  Pbx:hangup(),
  {next, Session}.
