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
    false -> download(Url, TargetPath)
  end,
  {file, Name}.

download(Url, TargetPath) ->
  {ok, {_, _, Body}} = httpc:request(Url),
  sox:convert(Body, TargetPath).
