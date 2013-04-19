-module(asterisk_pbx).
-export([new/1, answer/1, hangup/1, play/2, capture/6, terminate/1, sound_path_for/2]).

new(Pid) ->
  {?MODULE, Pid}.

sound_path_for(Name, _) ->
  "/usr/local/asterisk/var/lib/asterisk/sounds/verboice/" ++ Name ++ ".gsm".

terminate({?MODULE, Pid}) ->
  agi_session:close(Pid).

answer({?MODULE, Pid}) ->
  agi_session:answer(Pid).

hangup({?MODULE, Pid}) ->
  agi_session:hangup(Pid).

play(FileName, Pbx = {?MODULE, _Pid}) ->
  play(FileName, "", Pbx).

play(FileName, EscapeDigits, {?MODULE, Pid}) ->
  case agi_session:stream_file(Pid, "verboice/" ++ FileName, EscapeDigits) of
    {hangup, _} -> throw(hangup);
    Ret -> Ret
  end.

capture(FileName, Timeout, FinishOnKey, Min, Max, Pbx = {?MODULE, Pid}) ->
  case play(FileName, "0123456789#*", Pbx = {?MODULE, Pid}) of
    {ok, _} -> capture_digits(Timeout, FinishOnKey, Min, Max, Pid, "");
    {digit, Key, _} ->
      case lists:member(Key, FinishOnKey) of
        true -> finish_key;
        _Else -> capture_digits(Timeout, FinishOnKey, Min, Max, Pid, [Key])
      end
  end.

capture_digits(_Timeout, _FinishOnKey, _Min, Max, _Pid, Keys) when length(Keys) >= Max ->
  {digits, Keys};

capture_digits(Timeout, FinishOnKey, Min, Max, Pid, Keys) ->
  case agi_session:wait_for_digit(Pid, Timeout * 1000) of
    error -> throw(hangup);
    timeout when length(Keys) >= Min -> {digits, Keys};
    timeout -> timeout;
    {digit, Key} ->
      case lists:member(Key, FinishOnKey) of
        true ->
          if
            length(Keys) == 0 -> finish_key;
            length(Keys) < Min -> short_entry;
            true -> {digits, Keys}
          end;
        _ -> capture_digits(Timeout, FinishOnKey, Min, Max, Pid, Keys ++ [Key])
      end
  end.
