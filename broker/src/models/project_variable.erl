-module(project_variable).
-export([names_for_project/1]).
-define(CACHE, true).
-define(TABLE_NAME, "project_variables").
-include_lib("erl_dbmodel/include/model.hrl").

names_for_project(ProjectId) ->
  Vars = project_variable:find_all({project_id, ProjectId}),
  lists:map(fun(#project_variable{id = Id, name = Name}) ->
    VarName = binary_to_atom(<<"var_", Name/binary>>, utf8),
    {Id, VarName}
  end, Vars).
