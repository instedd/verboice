-module(pause).
-export([run/2]).
-include("session.hrl").

run(Args, Session) ->
  Length = proplists:get_value(length, Args),
  timer:sleep(timer:seconds(Length)),
  {next, Session}.
