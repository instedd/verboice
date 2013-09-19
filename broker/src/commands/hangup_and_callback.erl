-module(hangup_and_callback).
-export([run/2]).


run(_Args, Session) ->
  {suspend, Session}.
