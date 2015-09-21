-module(telemetry_util).
-export([call_ended/1]).

-include("db.hrl").

call_ended(undefined) ->
  ok;

call_ended(Project) when is_record(Project, project) ->
  Kind = <<"project_lifespan">>,
  Attributes = {[{<<"project_id">>, Project#project.id}]},
  {datetime, CreatedAt} = Project#project.created_at,
  telemetry:report(timespan_update, [Kind, Attributes, iso8601:format(CreatedAt)]).