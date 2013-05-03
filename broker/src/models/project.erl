-module(project).
-export([voice/2]).
-define(TABLE_NAME, "projects").

-define(MAP(Project),
  {ok, [Languages]} = yaml:load(Project#project.languages, [{schema, yaml_schema_ruby}]),
  Project#project{languages = Languages}
).

-include("model.hrl").

voice(Lang, #project{languages = Languages}) ->
  voice(Lang, Languages);

voice(_, []) -> undefined;
voice(Lang, [LangConfig | Rest]) ->
  case proplists:get_value("language", LangConfig) of
    Lang -> proplists:get_value("voice", LangConfig);
    _ -> voice(Lang, Rest)
  end.