-module(hangup).
-export([run/1]).
-include("session.hrl").

run(Session = #session{pbx = Pbx}) ->
  Pbx:hangup(),
  {next, Session}.