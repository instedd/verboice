-module(session_test).
-include_lib("eunit/include/eunit.hrl").
-include("session.hrl").
-include("db.hrl").

-define(setup(F), {setup, fun integration_test:start/0, fun integration_test:stop/1, F}).
-define(test(F), fun() -> integration_test:run_test_in_transaction(F) end).

-record(state, {session_id, session, resume_ptr, pbx_pid, flow_pid}).

notify_status_on_completed_ok_test() ->
  Session = #session{address = <<"123">>, call_log = {call_log_srv}, status_callback_url = <<"http://foo.com">>},
  meck:new(call_log_srv, [stub_all]),
  meck:expect(call_log_srv, id, 1, 1),
  meck:new(httpc),

  RequestParams = [get, {"http://foo.com/?CallSid=1&CallStatus=completed&From=123", []}, '_', [{full_result, false}]],
  meck:expect(httpc, request, RequestParams, ok),
  session:in_progress({completed, ok}, #state{session = Session}),

  meck:wait(httpc, request, RequestParams, 1000),
  meck:unload().

notify_status_on_completed_ok_with_callback_params_test() ->
  Session = #session{address = <<"123">>, call_log = {call_log_srv}, status_callback_url = <<"http://foo.com">>, callback_params = [{"foo", "1"}]},
  meck:new(call_log_srv, [stub_all]),
  meck:expect(call_log_srv, id, 1, 1),
  meck:new(httpc),

  RequestParams = [get, {"http://foo.com/?CallSid=1&CallStatus=completed&From=123&foo=1", []}, '_', [{full_result, false}]],
  meck:expect(httpc, request, RequestParams, ok),
  session:in_progress({completed, ok}, #state{session = Session}),

  meck:wait(httpc, request, RequestParams, 1000),
  meck:unload().

notify_status_with_http_credentials_test() ->
  Session = #session{address = <<"123">>, call_log = {call_log_srv}, status_callback_url = <<"http://foo.com">>, status_callback_user = "user", status_callback_password = "pass"},
  meck:new(call_log_srv, [stub_all]),
  meck:expect(call_log_srv, id, 1, 1),
  meck:new(httpc),

  RequestParams = [get, {"http://foo.com/?CallSid=1&CallStatus=completed&From=123", [{"Authorization", "Basic dXNlcjpwYXNz"}]}, '_', [{full_result, false}]],
  meck:expect(httpc, request, RequestParams, ok),
  session:in_progress({completed, ok}, #state{session = Session}),

  meck:wait(httpc, request, RequestParams, 1000),
  meck:unload().

run_simple_flow() ->
  Flow = call_flow:make([{broker_flow, flow:serialize([answer, hangup])}]),
  Channel = channel:make([{call_flow_id, Flow}]),
  {ok, SessionPid} = session:new(),
  Pbx = pbx_mock:new([
    {answer, [], ok},
    {hangup, [], ok}
  ]),

  session:answer(SessionPid, Pbx, Channel:id(), <<"1234">>),

  ?assertEqual(normal, integration_test:wait_process(SessionPid)),
  ?assertEqual(ok, Pbx:validate()).

run_queued_call() ->
  mock_broker:start(),

  Project = project:make(),
  Flow = call_flow:make([{project_id, Project}, {broker_flow, flow:serialize([answer, hangup])}]),
  Channel = channel:make([{call_flow_id, Flow}]),
  QueuedCall = queued_call:make([{project_id, Project}, {channel_id, Channel}, {call_flow_id, Flow}, {address, <<"123">>}]),
  scheduler:load(),

  SessionPid = mock_broker:wait_dispatch(QueuedCall#queued_call.id),

  Pbx = pbx_mock:new([
    {answer, [], ok},
    {hangup, [], ok}
  ]),
  session:answer(SessionPid, Pbx),

  ?assertEqual(normal, integration_test:wait_process(SessionPid)),
  ?assertEqual(ok, Pbx:validate()),

  meck:unload().

session_test_() ->
  ?setup([
    ?test(fun run_simple_flow/0),
    ?test(fun run_queued_call/0)
  ]).
