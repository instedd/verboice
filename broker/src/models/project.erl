-module(project).
-export([voice/2, tts_engine/1, ispeech_api_key/1, default_language/1, status_callback/1]).
-define(CACHE, true).
-define(TABLE_NAME, "projects").
-define(MAP, [
  {languages, yaml_serializer},
  {encrypted_config, encrypted_config_serializer}
]).
-include_lib("erl_dbmodel/include/model.hrl").

voice(Lang, #project{languages = Languages}) ->
  voice(Lang, Languages);

voice(_, []) -> undefined;
voice(Lang, Languages) when is_binary(Lang) ->
  voice(util:to_string(Lang), Languages);
voice(Lang, [LangConfig | Rest]) ->
  case proplists:get_value("language", LangConfig) of
    Lang -> proplists:get_value("voice", LangConfig);
    _ -> voice(Lang, Rest)
  end.

tts_engine(#project{encrypted_config = Config}) ->
  proplists:get_value("tts_engine", Config).

ispeech_api_key(#project{encrypted_config = Config}) ->
  proplists:get_value("tts_ispeech_api_key", Config).

default_language(#project{default_language = undefined, languages = [LangConfig | _]}) ->
  iolist_to_binary(proplists:get_value("language", LangConfig));
default_language(#project{default_language = undefined}) -> <<"en">>;
default_language(#project{default_language = Lang}) -> Lang.

status_callback(#project{status_callback_url = Url, encrypted_config = Config}) ->
  User = proplists:get_value("status_callback_url_user", Config),
  Pass = proplists:get_value("status_callback_url_password", Config),
  {Url, User, Pass}.
