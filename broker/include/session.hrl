-record(session, {
  session_id,
  pbx,
  flow,
  stack = [],
  call_flow,
  js_context,
  call_log,
  address,
  channel,
  queued_call,
  project,
  contact,
  default_language,
  status_callback_url,
  status_callback_user,
  status_callback_password,
  callback_params = [],
  in_user_step_activity = false,
  started_at
}).

-record(hibernated_session_data, {
  flow,
  stack,
  js_context,
  channel_id,
  call_flow,
  call_log_id,
  project_id,
  address,
  contact_id,
  default_language,
  status_callback_url,
  status_callback_user,
  status_callback_password,
  resume_ptr,
  poirot_activity
}).
