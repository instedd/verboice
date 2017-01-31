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
  meck:expect(project, status_callback, 1, {undefined, undefined, undefined, false}),

  Session = queued_call:start_session(QueuedCall),

  ?assertEqual(CallFlow, Session#session.call_flow),
  ?assertEqual(CallFlow#call_flow.broker_flow, Session#session.flow),

  meck:unload().

start_session_with_callback_url_test() ->
  QueuedCall = #queued_call{callback_url = <<"http://foo.com">>, project_id = project_id},
  meck:new(project),
  meck:expect(project, find, [project_id], #project{}),
  meck:expect(project, status_callback, 1, {undefined, undefined, undefined, false}),
  Session = queued_call:start_session(QueuedCall),

  ?assertEqual(undefined, Session#session.call_flow),
  ?assertEqual([answer,[callback,[{url,"http://foo.com"}]]], Session#session.flow),

  meck:unload().

start_session_with_flow_test() ->
  QueuedCall = #queued_call{flow = "<Response><Hangup/></Response>", project_id = project_id},
  meck:new(project),
  meck:expect(project, find, [project_id], #project{}),
  meck:expect(project, status_callback, 1, {undefined, undefined, undefined, false}),

  Session = queued_call:start_session(QueuedCall),

  ?assertEqual(undefined, Session#session.call_flow),
  ?assertEqual(twiml:parse(QueuedCall#queued_call.flow), Session#session.flow),

  meck:unload().

%% Test case for #780
start_session_with_invalid_flow_test() ->
  QueuedCall = #queued_call{flow = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n<Say voice=\"woman\">Hello.</Say>\n</Hangup>", project_id = project_id},
  meck:new(project),
  meck:expect(project, find, [project_id], #project{}),
  meck:expect(project, status_callback, 1, {undefined, undefined, undefined, false}),

  Session = queued_call:start_session(QueuedCall),

  ?assertEqual(undefined, Session#session.call_flow),
  ?assertEqual(undefined, Session#session.flow),

  meck:unload().

%% Test case for #780
start_session_with_invalid_xml_flow_test() ->
  QueuedCall = #queued_call{flow = "<Response>\n  <Gather numDigits=\"1\" action=\"http://twimlets.com/voicemail?Email=nicp%40textit.in&Message=https%3A%2F%2example.com%2F/voicemail.wav&Transcribe=false\">\n    <Say language=\"en\">Pick a number, zero through nine.</Say>\n  </Gather>\n</Response>", project_id = project_id},
  meck:new(project),
  meck:expect(project, find, [project_id], #project{}),
  meck:expect(project, status_callback, 1, {undefined, undefined, undefined, false}),

  Session = queued_call:start_session(QueuedCall),

  ?assertEqual(undefined, Session#session.call_flow),
  ?assertEqual(undefined, Session#session.flow),

  meck:unload().
