-record(channel, {id, account_id, call_flow_id, name, config, type, created_at, updated_at}).
-record(call_log, {id, account_id, project_id, finished_at, direction, address,
  state, created_at, updated_at, channel_id, started_at, schedule_id, not_before, call_flow_id, fail_reason}).
-record(call_log_entry, {id, call_id, severity, details, created_at, updated_at}).
-record(call_flow, {id, callback_url, flow, project_id, created_at, updated_at}).
-record(queued_call, {id, channel_id, call_log_id, address, callback_url, flow, call_flow_id, status_callback_url,
  schedule_id, not_before, retries, project_id, time_zone, variables, session_id, created_at, updated_at}).
-record(schedule, {id, name, retries, time_from, time_to, weekdays, project_id, created_at, updated_at}).
-record(project, {id, name, status_callback_url, default_language, languages, encrypted_config, created_at, updated_at}).
-record(resource, {id, name, project_id, guid, created_at, updated_at}).
-record(localized_resource, {id, language, text, recorded_audio, uploaded_audio, url, type, guid, resource_id, created_at, updated_at}).
-record(contact, {id, project_id, address, anonymous, created_at, updated_at}).
-record(persisted_variable, {id, contact_id, implicit_key, project_variable_id, value, created_at, updated_at}).
-record(project_variable, {id, project_id, name, created_at, updated_at}).
-record(recorded_audio, {id, contact_id, call_log_id, key, description, created_at, updated_at}).