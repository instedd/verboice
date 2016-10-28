-module(record).
-export([run/2]).
-include("session.hrl").
-include("db.hrl").

run(Args, Session = #session{pbx = Pbx, call_log = CallLog, contact = Contact, project = Project}) ->
  Key = util:to_string(proplists:get_value(key, Args)),
  Description = proplists:get_value(description, Args),
  StopKeys = proplists:get_value(stop_keys, Args, "01234567890*#"),
  Timeout = proplists:get_value(timeout, Args, 10),

  CallLogId = CallLog:id(),
  LocalFilename = local_filename(CallLogId, Key),
  AsteriskFilename = asterisk_filename(CallLogId, Key),
  filelib:ensure_dir(LocalFilename),

  poirot:log(info, "Recording to filename: ~s, stop keys: ~s, timeout: ~B, as: ~s", [LocalFilename, StopKeys, Timeout, AsteriskFilename]),
  case Pbx:record(AsteriskFilename, LocalFilename, StopKeys, Timeout) of
    ok ->
      RecordedAudio = #recorded_audio{
        contact_id = Contact#contact.id,
        project_id = Project#project.id,
        call_log_id = CallLogId,
        key = Key,
        description = Description
      },
      RecordedAudio:save(),

      {next, Session};

    {error, Reason} ->
      throw({error_recording, Reason})
  end.

filename(RecordDir, CallLogId, Key) ->
  filename:join([RecordDir, util:to_string(CallLogId), "results", Key ++ ".wav"]).

local_filename(CallLogId, Key) ->
  {ok, RecordDir} = application:get_env(record_dir),
  filename(RecordDir, CallLogId, Key).

asterisk_filename(CallLogId, Key) ->
  case application:get_env(asterisk_record_dir) of
    {ok, RecordDir} -> filename(RecordDir, CallLogId, Key);
    _ -> local_filename(CallLogId, Key)
  end.
