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
  {ok, {Key, _}} = Pbx:play(FileName, "0123456789#*"),
  IsFinishKey = lists:member(Key, FinishOnKey),
  case {IsFinishKey, Key} of
    {true, _} -> finish_key;
    {_, 0} -> capture_digits(Timeout, FinishOnKey, Min, Max, Pid, "");
    _ -> capture_digits(Timeout, FinishOnKey, Min, Max, Pid, [Key])
  end.

capture_digits(_Timeout, _FinishOnKey, _Min, Max, _Pid, Keys) when length(Keys) >= Max ->
  {digits, Keys};

capture_digits(Timeout, FinishOnKey, Min, Max, Pid, Keys) ->
  {ok, Key} = agi:wait_for_digit(Pid, Timeout * 1000),
  IsFinishKey = lists:member(Key, FinishOnKey),
  case {IsFinishKey, Key} of
    {_, -1} -> throw(hangup);
    {_, 0} when length(Keys) >= Min -> {digits, Keys};
    {_, 0} -> timeout;
    {true, _} ->
      if
        length(Keys) == 0 -> finish_key;
        length(Keys) < Min -> short_entry;
        true -> {digits, Keys}
      end;
    _ -> capture_digits(Timeout, FinishOnKey, Min, Max, Pid, Keys ++ [Key])
  end.
