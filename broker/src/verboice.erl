-module(verboice).
-export([main/1]).

main(X) ->
  application:start(eastrisk),
  application:start(verboice),
  io:format("~p~n", [X]).