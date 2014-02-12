-module(start_user_step).
-export([run/2]).
-include("session.hrl").
-include_lib("poirot_erlang/include/poirot.hrl").

run(Args, Session = #session{}) ->
  StepId = proplists:get_value(id, Args),
  StepName = proplists:get_value(name, Args),
  StepType = proplists:get_value(type, Args),

  case Session#session.in_user_step_activity of
    true -> poirot:pop();
    false -> ok
  end,

  StepActivity = poirot:new_activity("Step '~s'", [StepName]),
  poirot:push(StepActivity#activity{metadata = [
    {step_id, StepId},
    {step_name, list_to_binary(StepName)},
    {step_type, StepType}
  ]}),
  {next, Session#session{in_user_step_activity = true}}.
