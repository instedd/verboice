-module(asterisk_call_manager).
-behaviour(gen_event).

-export([
  code_change/3,
  handle_call/2,
  handle_event/2,
  handle_info/2,
  init/1,
  terminate/2
]).

code_change(_, _, _) -> {error}.
handle_call(_, _) -> {error}.
handle_info(_, _) -> {error}.
init(_) -> {ok, nil}.
terminate(_, _) -> {error}.

handle_event({new_channel, Pid, Env}, State) ->
  io:format("New call ~p~n", [Env]),
  Pbx = asterisk_pbx:new(Pid),
  session_srv:start(Pbx),
  {ok, State};

handle_event(_Event, State) ->
  {ok, State}.




