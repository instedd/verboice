-module(queued_call_test).
-include_lib("eunit/include/eunit.hrl").
-include("session.hrl").
-include("db.hrl").

start_session_with_call_flow_test() ->
  QueuedCall = #queued_call{call_flow_id = 123, project_id = project_id},
  CallFlow = #call_flow{broker_flow = [answer]},
  meck:new(call_flow, [passthrough]),
  meck:expect(call_flow, find, [123], CallFlow),
  meck:new(project),
  meck:expect(project, find, [project_id], #project{}),
  meck:expect(project, status_callback, 1, {undefined, undefined, undefined}),

  Session = queued_call:start_session(QueuedCall),

  ?assertEqual(CallFlow, Session#session.call_flow),
  ?assertEqual(CallFlow#call_flow.broker_flow, Session#session.flow),

  meck:unload().

start_session_with_callback_url_test() ->
  QueuedCall = #queued_call{callback_url = <<"http://foo.com">>, project_id = project_id},
  meck:new(project),
  meck:expect(project, find, [project_id], #project{}),
  meck:expect(project, status_callback, 1, {undefined, undefined, undefined}),
  Session = queued_call:start_session(QueuedCall),

  ?assertEqual(undefined, Session#session.call_flow),
  ?assertEqual([answer,[callback,[{url,"http://foo.com"}]]], Session#session.flow),

  meck:unload().

start_session_with_flow_test() ->
  QueuedCall = #queued_call{flow = [answer, hangup], project_id = project_id},
  meck:new(project),
  meck:expect(project, find, [project_id], #project{}),
  meck:expect(project, status_callback, 1, {undefined, undefined, undefined}),

  Session = queued_call:start_session(QueuedCall),

  ?assertEqual(undefined, Session#session.call_flow),
  ?assertEqual(QueuedCall#queued_call.flow, Session#session.flow),

  meck:unload().
