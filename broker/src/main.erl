-module(main).
-export([start/0]).

start() ->
  lager:start(),
  inets:start(),
  application:start(ernie_server),
  application:start(verboice).
