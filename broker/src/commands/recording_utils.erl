-module(recording_utils).
-export([asterisk_and_local_filenames/2]).

local_filename(CallLogId, Key) ->
  RecordDir = verboice_config:record_dir(),
  filename(RecordDir, CallLogId, Key).

asterisk_filename(CallLogId, Key) ->
  case verboice_config:asterisk_record_dir() of
    undefined -> local_filename(CallLogId, Key);
    RecordDir -> filename(RecordDir, CallLogId, Key)
  end.

filename(RecordDir, CallLogId, Key) ->
  filename:join([RecordDir, util:to_string(CallLogId), "results", Key ++ ".wav"]).

asterisk_and_local_filenames(CallLogId, Key) ->
  LocalFilename = local_filename(CallLogId, Key),
  AsteriskFilename = asterisk_filename(CallLogId, Key),
  filelib:ensure_dir(LocalFilename),
  [LocalFilename, AsteriskFilename].
