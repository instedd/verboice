-record(account, {id, email, created_at, updated_at}).
-record(call_flow, {id, callback_url, broker_flow, project_id, encrypted_config, store_in_fusion_tables, created_at, updated_at}).
-record(call_log, {id, account_id, project_id, finished_at, direction, address, state, created_at, updated_at, channel_id, started_at, schedule_id, not_before, call_flow_id, pbx_logs_guid, fail_reason, contact_id, fail_code, fail_details, callback_url, js_context}).
-record(call_log_entry, {id, call_id, severity, details, created_at, updated_at}).
-record(channel, {id, account_id, call_flow_id, name, config, type, created_at, updated_at, enabled}).
-record(contact, {id, project_id, anonymous, created_at, updated_at, last_activity_at}).
-record(contact_address, {id, address, contact_id, project_id, created_at, updated_at}).
-record(delayed_job, {id, handler, run_at, created_at, updated_at}).
-record(external_service, {id, project_id, guid, global_settings, created_at, updated_at}).
-record(impersonate_record, {id, call_flow_id, contact_id, impersonated_id, created_at, updated_at}).
-record(localized_resource, {id, language, text, recorded_audio, uploaded_audio, url, type, guid, resource_id, extras, created_at, updated_at}).
-record(persisted_variable, {id, contact_id, implicit_key, project_variable_id, value, created_at, updated_at}).
-record(pbx_log, {id, guid, details, created_at, updated_at}).
-record(project, {id, account_id, name, status_callback_url, status_callback_include_vars, default_language, languages, encrypted_config, created_at, updated_at}).
-record(project_variable, {id, project_id, name, created_at, updated_at}).
-record(queued_call, {id, channel_id, call_log_id, address, callback_url, flow, call_flow_id, status_callback_url, schedule_id, not_before, not_after, retries, project_id, time_zone, variables, session_id, callback_params, contact_id, scheduled_call_id, created_at, updated_at}).
-record(recorded_audio, {id, project_id, contact_id, call_log_id, key, description, created_at, updated_at}).
-record(resource, {id, name, project_id, guid, created_at, updated_at}).
-record(schedule, {id, name, retries, time_from, time_to, weekdays, project_id, created_at, updated_at}).
-record(trace_record, {id, call_flow_id, call_id, step_name, step_id, result, created_at, updated_at}).
-record(hibernated_session, {id, session_id, data, created_at, updated_at}).
-record(nuntium_channel, {id, channel_name, created_at, updated_at}).
-record(contact_scheduled_call, {id, contact_id, scheduled_call_id, last_called_at, created_at, updated_at}).
