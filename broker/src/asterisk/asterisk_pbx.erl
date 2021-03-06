-module(asterisk_pbx).
-export([new/1, pid/1, answer/1, reject/1, hangup/1, can_play/2, play/2, capture/6, record/5, terminate/1, sound_path_for/2, sound_quality/1, dial/6]).

-behaviour(pbx).

new(Pid) ->
  {?MODULE, Pid}.

pid({?MODULE, Pid}) -> Pid.

sound_path_for(Name, _) ->
  SoundsDir = verboice_config:asterisk_sounds_dir(),
  filename:join([SoundsDir, "verboice", Name ++ ".gsm"]).

sound_quality(_) ->
  "8000".

terminate({?MODULE, Pid}) ->
  agi_session:close(Pid).

answer({?MODULE, Pid}) ->
  agi_session:answer(Pid).

reject({?MODULE, Pid}) ->
  agi_session:hangup(Pid).

hangup({?MODULE, Pid}) ->
  agi_session:hangup(Pid).

can_play(file, _) -> true;
can_play(_, _) -> false.

play(Resource, Pbx = {?MODULE, _Pid}) ->
  play(Resource, "", Pbx).

play({file, FileName}, EscapeDigits, {?MODULE, Pid}) ->
  case agi_session:stream_file(Pid, "verboice/" ++ FileName, EscapeDigits) of
    hangup -> throw(hangup);
    {hangup, _} -> throw(hangup);
    Ret -> Ret
  end.

capture(Caption, Timeout, FinishOnKey, Min, Max, Pbx = {?MODULE, Pid}) ->
  %TODO: expect error
  case play(Caption, "0123456789#*", Pbx = {?MODULE, Pid}) of
    {ok, _} -> capture_digits(Timeout, FinishOnKey, Min, Max, Pid, "");
    {digit, Key, _} ->
      case lists:member(Key, FinishOnKey) of
        true -> finish_key;
        _Else -> capture_digits(Timeout, FinishOnKey, Min, Max, Pid, [Key])
      end;
    error -> throw({error, "Error during audio playback"})
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

record(AsteriskFilename, FileName, StopKeys, Timeout, {?MODULE, Pid}) ->
  TempFile = filename:rootname(FileName) ++ ".gsm",
  file:write_file(TempFile, <<>>),
  file:change_mode(TempFile, 8#666),

  try
    case agi_session:record_file(Pid, filename:absname(filename:rootname(AsteriskFilename)), "gsm", StopKeys, Timeout * 1000) of
      error -> throw(error);
      _ ->
        sox:convert(TempFile, FileName)
    end
  after
    file:delete(TempFile)
  end.

monitor_session(_Pid, undefined) ->
  undefined;
monitor_session(Pid, AsteriskFilename) ->
  agi_session:mix_monitor(Pid, [AsteriskFilename]).

stop_monitoring(_Pid, undefined, _LocalFilename) ->
  undefined;
stop_monitoring(Pid, AsteriskFilename, LocalFilename) ->
  agi_session:stop_mix_monitor(Pid),
  file:rename(AsteriskFilename, LocalFilename).

dial(Channel, Address, undefined, LocalFilename, AsteriskFilename, {?MODULE, Pid}) ->
  monitor_session(Pid, AsteriskFilename),
  DialAddress = case asterisk_broker:dial_address(Channel, Address) of
                  AsBinary when is_binary(AsBinary) -> binary_to_list(AsBinary);
                  AsList -> AsList
                end,
  agi_session:dial(Pid, [DialAddress, "60", "mg"]),
  stop_monitoring(Pid, AsteriskFilename, LocalFilename),
  case agi_session:get_variable(Pid, "DIALSTATUS") of
    hangup -> throw(hangup);
    {ok, Value} -> case Value of
      "ANSWER" -> completed;
      "BUSY" -> busy;
      "NOANSWER" -> no_answer;
      "CANCEL" -> throw(hangup);
      _ -> failed
    end
  end;
dial(Channel, Address, CallerId, LocalFilename, AsteriskFilename, Pbx = {?MODULE, Pid}) ->
  agi_session:set_callerid(Pid, CallerId),
  dial(Channel, Address, undefined, LocalFilename, AsteriskFilename, Pbx).
