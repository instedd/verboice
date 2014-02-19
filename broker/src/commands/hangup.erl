-module(hangup).
-export([run/1]).
-compile([{parse_transform, lager_transform}]).
-include("session.hrl").

run(Session = #session{pbx = Pbx}) ->
  lager:info("Hangup"),
  Pbx:hangup(),
  {next, Session}.
