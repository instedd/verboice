-module(yaml_serializer).
-export([load/1, dump/1]).

load(Yaml) when is_binary(Yaml) ->
  case yaml:load(Yaml, [{schema, yaml_schema_ruby}]) of
    {ok, [Doc]} -> Doc;
    _ -> []
  end;
load(_) -> [].

dump(Term) ->
  yaml:dump(Term, [{schema, yaml_schema_ruby}]).
