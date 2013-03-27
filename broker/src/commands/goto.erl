-module(goto).
-export([run/2]).
-include("session.hrl").

run(N, _Session) ->
  {goto, N}.