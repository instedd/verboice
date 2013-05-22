-module(default_cache).
-export([init/0, get/1, set/2]).

init() ->
  ets:new(default_cache, [set, public, named_table]).

get(Key) ->
  case ets:lookup(?MODULE, Key) of
    [{Key, Value}] -> Value;
    _ -> undefined
  end.

set(Key, Value) ->
  ets:insert(?MODULE, {Key, Value}).