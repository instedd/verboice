-module(main).
-export([start/0]).

start() ->
  application:start(verboice).
