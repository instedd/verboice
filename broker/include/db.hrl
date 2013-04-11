-record(channel, {id, account_id, call_flow_id, name, config, type}).
-record(call_log, {id, account_id, project_id, finished_at, direction, address,
  state, created_at, updated_at, channel_id, started_at, schedule_id, not_before, call_flow_id, fail_reason}).