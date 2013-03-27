-module(asterisk_pbx).
-export([new/1, answer/1, play/2, capture/6, terminate/1, sound_path_for/2]).

new(Pid) ->
  {?MODULE, Pid}.

sound_path_for(Name, _) ->
  "/usr/local/asterisk/var/lib/asterisk/sounds/verboice/" ++ Name ++ ".gsm".

terminate({?MODULE, Pid}) ->
  agi_channel:close(Pid).

answer({?MODULE, Pid}) ->
  io:format("PID: ~p~n", [Pid]),
  agi:answer(Pid).

play(FileName, Pbx = {?MODULE, _Pid}) ->
  play(FileName, "", Pbx).

play(FileName, EscapeDigits, {?MODULE, Pid}) ->
  case agi:stream_file(Pid, "verboice/" ++ FileName, EscapeDigits) of
    {ok, "-1"} -> throw(hangup);
    Ret -> Ret
  end.

capture(FileName, Timeout, FinishOnKey, Min, Max, Pbx = {?MODULE, Pid}) ->
  {ok, {Key, _}} = play(FileName, "0123456789#*", Pbx = {?MODULE, Pid}),
  case Key of
    FinishOnKey -> finish_key;
    0 -> capture_digits(Timeout, FinishOnKey, Min, Max, Pid, "");
    _ -> capture_digits(Timeout, FinishOnKey, Min, Max, Pid, Key)
  end.

capture_digits(_Timeout, _FinishOnKey, _Min, Max, _Pid, Keys) when length(Keys) >= Max ->
  {digits, Keys};

capture_digits(Timeout, FinishOnKey, Min, Max, Pid, Keys) ->

  {ok, Digit} = agi:wait_for_digit(Pid, Timeout * 1000),
  io:format("~p~n", [Digit]),
  case Digit of
    -1 -> throw(hangup);
    0 when length(Keys) >= Min -> {digits, Keys};
    0 -> timeout;
    FinishOnKey when length(Keys) == 0 -> finish_key;
    FinishOnKey when length(Keys) < Min -> short_entry;
    FinishOnKey -> {digits, Keys};
    Key -> capture_digits(Timeout, FinishOnKey, Min, Max, Pid, Keys ++ [Key])
  end.
