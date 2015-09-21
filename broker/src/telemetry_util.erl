-module(telemetry_util).
-export([call_ended/1]).

-include("db.hrl").

call_ended(Project) ->
  Kind = <<"project_lifespan">>,
  Attributes = {[{<<"project_id">>, Project#project.id}]},
  {datetime, CreatedAt} = Project#project.created_at,
  telemetry:report(timespan_update, [Kind, Attributes, iso8601:format(CreatedAt)]).