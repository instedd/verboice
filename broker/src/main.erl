-module(main).
-export([start/0]).

start() ->
  application:ensure_all_started(verboice, permanent),
  application:ensure_all_started(ernie_server, permanent).
