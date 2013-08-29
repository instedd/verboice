-module(session_test).
-include_lib("eunit/include/eunit.hrl").
-include("session.hrl").
-include("db.hrl").

-record(state, {session_id, session, resume_ptr, pbx_pid, flow_pid}).

notify_status_on_completed_ok_test() ->
  Project = #project{status_callback_url = <<"http://foo.com">>},
  Session = #session{session_id = "1", address = <<"123">>, call_log = #call_log{}, project = Project},
  meck:new(call_log, [stub_all]),
  meck:new(httpc),

  RequestParams = [get, {"http://foo.com/?CallSid=1&CallStatus=completed&From=123", []}, '_', [{full_result, false}]],
  meck:expect(httpc, request, RequestParams, ok),
  session:in_progress({completed, ok}, #state{session = Session}),

  meck:wait(httpc, request, RequestParams, 1000),
  meck:unload().

notify_status_on_completed_ok_with_callback_params_test() ->
  Project = #project{status_callback_url = <<"http://foo.com">>},
  Session = #session{session_id = "1", address = <<"123">>, call_log = #call_log{}, project = Project, callback_params = [{"foo", "1"}]},
  meck:new(call_log, [stub_all]),
  meck:new(httpc),

  RequestParams = [get, {"http://foo.com/?CallSid=1&CallStatus=completed&From=123&foo=1", []}, '_', [{full_result, false}]],
  meck:expect(httpc, request, RequestParams, ok),
  session:in_progress({completed, ok}, #state{session = Session}),

  meck:wait(httpc, request, RequestParams, 1000),
  meck:unload().
