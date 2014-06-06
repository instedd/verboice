-module(start_activity).
-export([run/2]).
-include("session.hrl").
-include_lib("poirot/include/poirot.hrl").

run(Args, Session = #session{call_log = CallLog}) ->
  Name = proplists:get_value(name, Args),
  Metadata = proplists:get_value(metadata, Args, []),

  case Session#session.in_user_step_activity of
    true -> poirot:pop();
    false -> ok
  end,

  StepActivity = poirot:new_activity(Name),
  poirot:push(StepActivity#activity{metadata = [{call_log_id, CallLog:id()} | util:to_poirot(Metadata)]}),
  {next, Session#session{in_user_step_activity = true}}.
