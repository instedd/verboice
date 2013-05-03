-module(resource).
-export([find_by_guid/1, localized_resource/2, prepare/2, prepare_text_resource/2, prepare_blob_resource/3, prepare_url_resource/2]).
-define(TABLE_NAME, "resources").
-include("model.hrl").
-include("session.hrl").

find_by_guid(Guid) ->
  find({guid, Guid}).

localized_resource(Language, #resource{id = Id}) ->
  localized_resource:find([{resource_id, Id}, {language, Language}]).

prepare(Guid, Session) ->
  Resource = find_by_guid(Guid),
  LocalizedResource = Resource:localized_resource(Session:language()),
  LocalizedResource:prepare(Session).

prepare_text_resource(Text, #session{pbx = Pbx}) ->
  case Pbx:can_play(text) of
    false ->
      Hash = crypto:md5(Text),
      Name = lists:flatten([io_lib:format("~2.16.0b", [B]) || <<B>> <= Hash]),

      TargetPath = Pbx:sound_path_for(Name),
      case filelib:is_file(TargetPath) of
        true -> ok;
        false -> synthesize(Text, TargetPath)
      end,
      {file, Name};
    true ->
      {text, Text}
  end.

prepare_blob_resource(Name, Blob, #session{pbx = Pbx}) ->
  TargetPath = Pbx:sound_path_for(Name),
  sox:convert(Blob, TargetPath),
  {file, Name}.

prepare_url_resource(Url, #session{pbx = Pbx}) ->
  Hash = crypto:md5(Url),
  Name = lists:flatten([io_lib:format("~2.16.0b", [B]) || <<B>> <= Hash]),

  TargetPath = Pbx:sound_path_for(Name),
  case filelib:is_file(TargetPath) of
    true -> ok;
    false -> download(Url, TargetPath)
  end,
  {file, Name}.

synthesize(Text, TargetPath) ->
  TempFile = TargetPath ++ ".wave",
  try
    Port = open_port({spawn, "say -v Paulina -o " ++ TempFile}, [binary]),
    port_command(Port, Text),
    port_close(Port),
    ok = wait_for_file(TempFile),
    sox:convert(TempFile, TargetPath)
  after
    file:delete(TempFile)
  end.

download(Url, TargetPath) ->
  {ok, {_, _, Body}} = httpc:request(Url),
  sox:convert(Body, TargetPath).

wait_for_file(FilePath) -> wait_for_file(FilePath, 5).
wait_for_file(_, 0) -> timeout;
wait_for_file(FilePath, N) ->
  case filelib:is_file(FilePath) of
    true -> ok;
    false -> timer:sleep(50), wait_for_file(FilePath, N - 1)
  end.
