-module(asterisk_pbx).
-export([new/1, answer/1, play/2, terminate/1]).

new(Pid) ->
  {?MODULE, Pid}.

terminate({?MODULE, Pid}) ->
  agi_channel:close(Pid).

answer({?MODULE, Pid}) ->
  io:format("PID: ~p~n", [Pid]),
  agi:answer(Pid).

play(FileName, {?MODULE, Pid}) ->
  case agi:stream_file(Pid, "verboice/" ++ FileName, "") of
    {ok, "-1"} -> throw(hangup);
    X ->
      io:format("~p~n", [X]),
      ok
  end.