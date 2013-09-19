-module(delayed_job).
-define(TABLE_NAME, "delayed_jobs").
-export([enqueue/1]).
-include_lib("erl_dbmodel/include/model.hrl").

enqueue(Task) ->
  create(#delayed_job{handler = Task, run_at = {datetime, calendar:universal_time()}}).
