-module(asterisk_pbx).
-export([new/1, answer/1, play/2, terminate/1, sound_path_for/2]).

new(Pid) ->
  {?MODULE, Pid}.

sound_path_for(Name, _) ->
  "/usr/local/asterisk/var/lib/asterisk/sounds/verboice/" ++ Name ++ ".gsm".

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