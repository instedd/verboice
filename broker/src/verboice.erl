-module(verboice).
-export([main/1]).

main(X) ->
  application:start(verboice),
  io:format("~p~n", [X]).