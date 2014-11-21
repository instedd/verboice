-module(delayed_job).
-define(TABLE_NAME, "delayed_jobs").
-export([enqueue/1, enqueue/2]).
-include_lib("erl_dbmodel/include/model.hrl").

enqueue(Task) ->
  enqueue(Task, 0).

enqueue(Task, DelaySeconds) ->
  NowSeconds = calendar:datetime_to_gregorian_seconds(calendar:universal_time()),
  RunAtSeconds = NowSeconds + DelaySeconds,
  RunAt = {datetime, calendar:gregorian_seconds_to_datetime(RunAtSeconds)},
  create(#delayed_job{handler = Task, run_at = RunAt}).

