-module(persist_variable).
-export([run/2]).
-include("session.hrl").
-include("db.hrl").

run(Args, Session = #session{js_context = JS}) ->
  Name = proplists:get_value(name, Args),
  Expression = proplists:get_value(expression, Args),
  {Value, JS2} = erjs:eval(Expression, JS),

  PersistedVar = (find_or_create_persisted_variable(Name, Session))#persisted_variable{value = Value},
  poirot:log(info, "Saving variable '~s' with value: ~s", [Name, Value]),
  PersistedVar:save(),

  VarName = list_to_atom("var_" ++ Name),
  JS3 = erjs_context:set(VarName, Value, JS2),
  {next, Session#session{js_context = JS3}}.

find_or_create_persisted_variable(Name, #session{contact = Contact, project = Project}) ->
  case Name of
    "language" ->
      persisted_variable:find_or_new([
        {contact_id, Contact#contact.id},
        {implicit_key, "language"}
      ]);
    _ ->
      #project_variable{id = ProjectVarId} = project_variable:find([{project_id, Project#project.id}, {name, Name}]),
      persisted_variable:find_or_new([
        {contact_id, Contact#contact.id},
        {project_variable_id, ProjectVarId}
      ])
  end.
