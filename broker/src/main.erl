-module(main).
-export([start/0]).

start() ->
  lager:start(),
  inets:start(),
  ssl:start(),
  application:start(ernie_server),
  application:start(verboice).
