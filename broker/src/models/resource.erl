-module(resource).
-export([localized_resource/2, prepare/2, prepare/3, prepare_text_resource/2, prepare_text_resource/3, prepare_blob_resource/5, prepare_url_resource/2]).
-define(CACHE, true).
-define(TABLE_NAME, "resources").
-include_lib("erl_dbmodel/include/model.hrl").
-include("session.hrl").
-compile([{parse_transform, lager_transform}]).

find_by_project_and_guid(ProjectId, Guid) ->
  find([{project_id, ProjectId}, {guid, Guid}]).

localized_resource(Language, #resource{id = Id}) ->
  localized_resource:find([{resource_id, Id}, {language, Language}]).

prepare(Guid, Session) ->
  prepare(Guid, Session, Session:language()).

prepare(Guid, Session = #session{project = #project{id = ProjectId}}, Language) ->
  Resource = find_by_project_and_guid(ProjectId, Guid),
  case Resource:localized_resource(Language) of
    undefined -> exit(resource_undefined);
    LocalizedResource -> LocalizedResource:prepare(Session)
  end.

prepare_text_resource(Text, Session) ->
  prepare_text_resource(Text, undefined, Session).

prepare_text_resource(Text, undefined, Session) ->
  prepare_text_resource(Text, Session:language(), Session);
prepare_text_resource(Text, Language, #session{pbx = Pbx, project = Project, js_context = JsContext}) ->
  ReplacedText = util:interpolate(Text, fun(JsCode) ->
    {Value, _} = erjs:eval(JsCode, JsContext),
    if
      is_integer(Value) -> list_to_binary(integer_to_list(Value));
      is_list(Value) -> list_to_binary(Value);
      true -> <<>>
    end
  end),
  case Pbx:can_play({text, Language}) of
    false ->
      Name = file_name(Project, Language, ReplacedText),
      TargetPath = Pbx:sound_path_for(Name),
      case filelib:is_file(TargetPath) of
        true ->
          poirot:log(info, "Audio file already exists, no need to synthesize"),
          ok;
        false ->
          poirot:log(info, "Synthesizing to ~s", [TargetPath]),
          case tts:synthesize(ReplacedText, Project, Language, TargetPath) of
            ok -> ok;
            Error ->
              poirot:log(warn, "Error synthesizing audio resource: ~p", [Error]),
              throw({error, "Could not synthesize audio resource"})
          end
      end,
      {file, Name};
    true ->
      {text, Language, ReplacedText}
  end.

file_name(Project, Language, Text) ->
  ProjectId = Project#project.id,
  LanguageBin = util:as_binary(Language),
  Timestamp = case Project#project.updated_at of
    {datetime, DateTime} -> calendar:datetime_to_gregorian_seconds(DateTime);
    _                    -> 0
  end,
  CacheData = <<ProjectId/integer, LanguageBin/binary, Timestamp/integer, Text/binary>>,
  util:md5hex(CacheData).

prepare_blob_resource(BlobName, UpdatedAt, Blob, Extension, #session{pbx = Pbx, project = Project}) ->
  Name = integer_to_list(Project#project.id) ++ "-" ++ BlobName,
  TargetPath = Pbx:sound_path_for(Name),
  case must_update(TargetPath, UpdatedAt) of
    false -> ok;
    true ->
      {A, B, C} = now(),
      FileName = lists:flatten(io_lib:format("/tmp/verboice-resource-~p~p~p~s", [A, B, C, Extension])),
      file:write_file(FileName, Blob),
      Quality = Pbx:sound_quality(),
      convert_and_delete_with_quality(FileName, Quality, TargetPath)
  end,
  {file, Name}.

prepare_url_resource(Url, #session{pbx = Pbx}) ->
  case Pbx:can_play(url) of
    true -> {url, Url};
    _ ->
      Name = util:md5hex(Url),
      TargetPath = Pbx:sound_path_for(Name),
      case filelib:is_file(TargetPath) of
        true -> ok;
        false ->
          TempFile = TargetPath ++ ".tmp",
          {ok, {_, Headers, Body}} = httpc:request(Url),
          file:write_file(TempFile, Body),
          Type = guess_type(Url, Headers),
          convert_and_delete(TempFile, Type, TargetPath)
      end,
      {file, Name}
  end.

convert_and_delete(TempFile, Type, TargetPath) ->
  try
    SoxResult = case Type of
      undefined -> sox:convert(TempFile, TargetPath);
      _ -> sox:convert(TempFile, Type, TargetPath)
    end,
    case SoxResult of
      error -> throw("Failed to convert audio file");
      _ -> ok
    end
  after
    file:delete(TempFile)
  end.

convert_and_delete_with_quality(TempFile, Quality, TargetPath) ->
  try
    SoxResult = sox:convert_with_quality(TempFile, Quality, TargetPath),
    case SoxResult of
      error -> throw("Failed to convert audio file");
      _ -> ok
    end
  after
    file:delete(TempFile)
  end.

guess_type(Url, Headers) ->
  case proplists:get_value("content-type", Headers) of
    "audio/mpeg" -> "mp3";
    "audio/mpeg3" -> "mp3";
    "audio/x-mpeg-3" -> "mp3";
    "audio/wav" -> "wav";
    "audio/x-wav" -> "wav";
    _ -> case filename:extension(Url) of
      ".wav" -> "wav";
      ".mp3" -> "mp3";
      ".gsm" -> "gsm";
      _ -> throw("Unknown file type")
    end
  end.

must_update(FileName, UpdatedAt) ->
  case filelib:last_modified(FileName) of
    0 -> true;
    LastModified ->
      calendar:universal_time_to_local_time(UpdatedAt) > LastModified
  end.
