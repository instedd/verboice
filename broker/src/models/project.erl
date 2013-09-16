-module(project).
-export([voice/2, tts_engine/1, ispeech_api_key/1]).
-define(CACHE, true).
-define(TABLE_NAME, "projects").

-define(MAP(Project),
  Languages = case Project#project.languages of
    undefined -> [];
    LanguagesYaml ->
      {ok, [X]} = yaml:load(LanguagesYaml, [{schema, yaml_schema_ruby}]), X
  end,
  Config = case Project#project.encrypted_config of
    undefined -> [];
    CryptConfig ->
      {ok, Secret} = application:get_env(verboice, crypt_secret),
      [PlainConfig] = marshal:decode(aes:decrypt(Secret, base64:decode(CryptConfig))),
      PlainConfig
  end,
  Project#project{languages = Languages, encrypted_config = Config}
).

-include_lib("erl_dbmodel/include/model.hrl").

voice(Lang, #project{languages = Languages}) ->
  voice(Lang, Languages);

voice(_, []) -> undefined;
voice(Lang, [LangConfig | Rest]) ->
  case proplists:get_value("language", LangConfig) of
    Lang -> proplists:get_value("voice", LangConfig);
    _ -> voice(Lang, Rest)
  end.

tts_engine(#project{encrypted_config = Config}) ->
  proplists:get_value("tts_engine", Config).

ispeech_api_key(#project{encrypted_config = Config}) ->
  proplists:get_value("tts_ispeech_api_key", Config).
