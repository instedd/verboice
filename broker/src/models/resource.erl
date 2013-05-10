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
  case Resource:localized_resource(Session:language()) of
    not_found -> exit(resource_not_found);
    LocalizedResource -> LocalizedResource:prepare(Session)
  end.

replace_vars(Text, Session) -> replace_vars(Text, Session, <<>>).

replace_vars(<<>>, _, Output) -> Output;
replace_vars(Text, Session, Output) ->
  case binary:split(Text, [<<${>>]) of
    [_] -> <<Output/binary, Text/binary>>;
    [H1, T1] ->
      case binary:split(T1, [<<$}>>]) of
        [_] -> <<Output/binary, Text/binary>>;
        [VarNameBin, T2] ->
          VarName = binary_to_atom(<<"var_", VarNameBin/binary>>, utf8),
          Value = case erjs_object:get(VarName, Session#session.js_context) of
            undefined -> <<>>;
            X -> list_to_binary(X)
          end,
          replace_vars(T2, Session, <<Output/binary, H1/binary, Value/binary>>)
      end
  end.

prepare_text_resource(Text, Session = #session{pbx = Pbx, project = Project}) ->
  ReplacedText = replace_vars(Text, Session),
  Language = Session:language(),
  case Pbx:can_play({text, Language}) of
    false ->
      Name = util:md5hex(ReplacedText),
      TargetPath = Pbx:sound_path_for(Name),
      case filelib:is_file(TargetPath) of
        true -> ok;
        false -> tts:synthesize(ReplacedText, Project, Language, TargetPath)
      end,
      {file, Name};
    true ->
      {text, Language, ReplacedText}
  end.

prepare_blob_resource(Name, Blob, #session{pbx = Pbx}) ->
  TargetPath = Pbx:sound_path_for(Name),
  sox:convert(Blob, TargetPath),
  {file, Name}.

prepare_url_resource(Url, #session{pbx = Pbx}) ->
  Name = util:md5hex(Url),
  TargetPath = Pbx:sound_path_for(Name),
  case filelib:is_file(TargetPath) of
    true -> ok;
    false ->
      TempFile = TargetPath ++ ".tmp",
      try
        {ok, {_, Headers, Body}} = httpc:request(Url),
        file:write_file(TempFile, Body),
        Type = guess_type(Url, Headers),
        sox:convert(TempFile, Type, TargetPath)
      after
        file:delete(TempFile)
      end
  end,
  {file, Name}.

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
