-module(blueprint).
-export([make/2]).

make(call_flow, _) -> [
  {project_id, project}
];

make(project, _) -> [
  {name, fun make_ref/0 },
  {default_language, "en"}
];

make(channel, _) -> [
  {name, fun make_ref/0 },
  {config, []}
];

make(queued_call, _) -> [
  {call_log_id, call_log},
  {variables, []}
];

make(call_log, _) -> [
];

make(resource, _) -> [
  {name, fun make_ref/0},
  {guid, uuid()}
];

make(localized_resource, text) -> [
  {type, "TextLocalizedResource"},
  {language, "en"},
  {resource_id, resource},
  {guid, uuid()}
];

make(Model, _) -> throw({missing_blueprint, Model}).

uuid() ->
  list_to_binary(uuid:to_string(uuid:v4())).
