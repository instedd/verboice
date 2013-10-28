-module(external_service).
-export([global_variable_value_for/2]).
-define(CACHE, true).
-define(TABLE_NAME, "external_services").
-define(MAP, [{global_settings, yaml_serializer}]).
-include_lib("erl_dbmodel/include/model.hrl").

global_variable_value_for(Name, #external_service{global_settings = GlobalSettings}) ->
  case proplists:get_value(variables, GlobalSettings) of
    undefined -> undefined;
    Variables -> variable_value(Name, Variables)
  end.


variable_value(_, []) -> undefined;
variable_value(Name, [Var | Rest]) ->
  case proplists:get_value("name", Var) of
    Name -> proplists:get_value("value", Var);
    _ -> variable_value(Name, Rest)
  end.
