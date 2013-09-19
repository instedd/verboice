-module(goto).
-export([run/2]).
-include("session.hrl").

run(N, Session) ->
  {{goto, N}, Session}.