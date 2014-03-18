-module(record).
-export([run/2]).
-compile([{parse_transform, lager_transform}]).
-include("session.hrl").
-include("db.hrl").

run(Args, Session = #session{pbx = Pbx, call_log = CallLog, contact = Contact, project = Project}) ->
  Key = util:to_string(proplists:get_value(key, Args)),
  Description = proplists:get_value(description, Args),
  StopKeys = proplists:get_value(stop_keys, Args, "01234567890*#"),
  Timeout = proplists:get_value(timeout, Args, 10),

  CallLogId = CallLog:id(),
  Filename = filename(CallLogId, Key),
  filelib:ensure_dir(Filename),

  lager:info("Recording to filename: ~s, stop keys: ~s, timeout: ~B", [Filename, StopKeys, Timeout]),
  Pbx:record(Filename, StopKeys, Timeout),

  RecordedAudio = #recorded_audio{
    contact_id = Contact#contact.id,
    project_id = Project#project.id,
    call_log_id = CallLogId,
    key = Key,
    description = Description
  },
  RecordedAudio:save(),

  {next, Session}.

filename(CallLogId, Key) ->
  {ok, RecordDir} = application:get_env(record_dir),
  filename:join([RecordDir, util:to_string(CallLogId), "results", Key ++ ".wav"]).
