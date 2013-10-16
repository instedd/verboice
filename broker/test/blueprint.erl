-module(blueprint).
-export([make/1]).

make(call_flow) -> [
  {project_id, project}
];

make(project) -> [
  {name, fun make_ref/0 },
  {default_language, "en"}
];

make(channel) -> [
  {name, fun make_ref/0 },
  {config, <<"--- \n...\n">>}
];

make(queued_call) -> [
  {call_log_id, call_log},
  {variables, <<"--- \n...\n">>}
];

make(call_log) -> [
];

make(Model) -> throw({missing_blueprint, Model}).
