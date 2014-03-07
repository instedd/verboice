-module(hibernated_session).
-export([wake_up/1]).
-define(TABLE_NAME, "hibernated_sessions").
-define(MAP, [
  {data, binary_serializer}
]).
-include_lib("erl_dbmodel/include/model.hrl").
-include("session.hrl").

wake_up(#hibernated_session{session_id = SessionId, data = #hibernated_session_data{
    channel_id = ChannelId,
    flow = Flow,
    call_flow = CallFlow,
    call_log_id = CallLogId,
    project_id = ProjectId,
    address = Address,
    contact_id = ContactId,
    status_callback_url = StatusCallbackUrl,
    status_callback_user = StatusCallbackUser,
    status_callback_password = StatusCallbackPassword
  }}) ->
  #session{
    session_id = SessionId,
    channel = channel:find(ChannelId),
    flow = Flow,
    call_flow = CallFlow,
    call_log = call_log:find(CallLogId),
    project = project:find(ProjectId),
    address = Address,
    contact = contact:find(ContactId),
    status_callback_url = StatusCallbackUrl,
    status_callback_user = StatusCallbackUser,
    status_callback_password = StatusCallbackPassword
  }.
