-module(main).
-export([start/0]).

start() ->
  inets:start(),
  application:start(verboice).
