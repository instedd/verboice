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

prepare_text_resource(Text, Session = #session{pbx = Pbx, project = Project}) ->
  case Pbx:can_play(text) of
    false ->
      Name = util:md5hex(Text),
      TargetPath = Pbx:sound_path_for(Name),
      case filelib:is_file(TargetPath) of
        true -> ok;
        false -> tts:synthesize(Text, Project, Session:language(), TargetPath)
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
