-module(functional_test).
-include_lib("eunit/include/eunit.hrl").

% -define(setup(F), {setup, fun test_app:start/0, fun test_app:stop/1, F}).
-define(setup(F), lists:map(fun(TestDefinition) -> {setup, fun test_app:start/0, fun test_app:stop/1, TestDefinition} end, F)).
-define(test(F), {atom_to_list(F), fun() ->
  try
    test_app:run_test_in_transaction(fun F/0)
  after
    meck:unload()
  end
end}).
-include("db.hrl").

session_test_() ->
  ?setup([
    ?test(run_simple_flow),
    ?test(run_queued_call),
    ?test(run_queued_call_if_not_before_already_passed),
    ?test(dont_run_queued_call_if_not_before),
    ?test(run_queued_call_if_before_not_after),
    ?test(dont_run_queued_call_if_not_after),
    ?test(play_resource_with_default_language),
    ?test(run_concurrent_calls),
    ?test(unanswered_call),
    ?test(unexpected_error)
  ]).

run_simple_flow() ->
  Flow = call_flow:make([{broker_flow, [answer, hangup]}]),
  Channel = channel:make([{call_flow_id, Flow}]),
  {ok, SessionPid} = session:new(),
  Pbx = pbx_mock:new([
    {answer, [], ok},
    {hangup, [], ok}
  ]),

  session:answer(SessionPid, Pbx, Channel:id(), <<"1234">>),

  ?assertEqual(normal, test_app:wait_process(SessionPid)),
  ?assertEqual(ok, Pbx:validate()).

run_queued_call() ->
  mock_broker:start(),

  Project = project:make(),
  Flow = call_flow:make([{project_id, Project}, {broker_flow, [answer, hangup]}]),
  Channel = channel:make([{call_flow_id, Flow}]),
  QueuedCall = queued_call:make([{project_id, Project}, {channel_id, Channel}, {call_flow_id, Flow}, {address, <<"123">>}]),
  scheduler:load(),

  SessionPid = mock_broker:wait_dispatch(QueuedCall#queued_call.id),

  Pbx = pbx_mock:new([
    {answer, [], ok},
    {hangup, [], ok}
  ]),
  session:answer(SessionPid, Pbx),

  ?assertEqual(normal, test_app:wait_process(SessionPid)),
  ?assertEqual(ok, Pbx:validate()).

run_queued_call_if_not_before_already_passed() ->
  mock_broker:start(),

  Project = project:make(),
  Flow = call_flow:make([{project_id, Project}, {broker_flow, [answer, hangup]}]),
  Channel = channel:make([{call_flow_id, Flow}]),
  QueuedCall = queued_call:make([{project_id, Project}, {channel_id, Channel}, {call_flow_id, Flow}, {address, <<"123">>}, {not_before, util:time_from_now(-60)}]),
  scheduler:load(),

  SessionPid = mock_broker:wait_dispatch(QueuedCall#queued_call.id),

  Pbx = pbx_mock:new([
    {answer, [], ok},
    {hangup, [], ok}
  ]),
  session:answer(SessionPid, Pbx),

  ?assertEqual(normal, test_app:wait_process(SessionPid)),
  ?assertEqual(ok, Pbx:validate()).

dont_run_queued_call_if_not_before() ->
  mock_broker:start(),
  meck:new(channel_queue),

  Project = project:make(),
  Flow = call_flow:make([{project_id, Project}, {broker_flow, [answer, hangup]}]),
  Channel = channel:make([{call_flow_id, Flow}]),
  NotBefore = util:time_from_now(6000),
  QueuedCall = queued_call:make([{project_id, Project}, {channel_id, Channel}, {call_flow_id, Flow}, {address, <<"123">>}, {not_before, NotBefore}]),
  scheduler:load(),
  Id = QueuedCall#queued_call.id,

  QueuedCalls = gen_server:call(scheduler, get_queued_calls),
  ?assertMatch([{NotBefore, #queued_call{id = Id}}], QueuedCalls).

run_queued_call_if_before_not_after() ->
  mock_broker:start(),

  Project = project:make(),
  Flow = call_flow:make([{project_id, Project}, {broker_flow, [answer, hangup]}]),
  Channel = channel:make([{call_flow_id, Flow}]),
  QueuedCall = queued_call:make([{project_id, Project}, {channel_id, Channel}, {call_flow_id, Flow}, {address, <<"123">>}, {not_after, util:time_from_now(600)}]),
  scheduler:load(),

  SessionPid = mock_broker:wait_dispatch(QueuedCall#queued_call.id),

  Pbx = pbx_mock:new([
    {answer, [], ok},
    {hangup, [], ok}
  ]),
  session:answer(SessionPid, Pbx),

  ?assertEqual(normal, test_app:wait_process(SessionPid)),
  ?assertEqual(ok, Pbx:validate()).

dont_run_queued_call_if_not_after() ->
  mock_broker:start(),
  meck:new(channel_queue),

  Project = project:make(),
  Flow = call_flow:make([{project_id, Project}, {broker_flow, [answer, hangup]}]),
  Channel = channel:make([{call_flow_id, Flow}]),
  NotAfter = util:time_from_now(-6000),
  queued_call:make([{project_id, Project}, {channel_id, Channel}, {call_flow_id, Flow}, {address, <<"123">>}, {not_after, NotAfter}]),
  scheduler:load(),

  QueuedCalls = gen_server:call(scheduler, get_queued_calls),
  ?assertEqual([], QueuedCalls).

play_resource_with_default_language() ->
  Project = project:make(),
  Resource = resource:make([{project_id, Project}]),
  localized_resource:make(text, [{resource_id, Resource}, {text, "hello"}]),
  Flow = call_flow:make([{project_id, Project}, {broker_flow, [answer, [play_resource, [{resource_guid, Resource#resource.guid}]]]}]),
  Channel = channel:make([{call_flow_id, Flow}]),
  {ok, SessionPid} = session:new(),
  Pbx = pbx_mock:new([
    {answer, [], ok},
    {can_play, [{text, "en"}], true},
    {play, [{text, "en", <<"hello">>}], ok}
  ]),

  session:answer(SessionPid, Pbx, Channel:id(), <<"1234">>),

  ?assertEqual(normal, test_app:wait_process(SessionPid)),
  ?assertEqual(ok, Pbx:validate()).

run_concurrent_calls() ->
  mock_broker:start(),

  Project = project:make(),
  Flow = call_flow:make([{project_id, Project}, {broker_flow, [answer, hangup]}]),
  Channel = channel:make([{call_flow_id, Flow}, {config, [{"limit", "2"}]}]),
  QueuedCall1 = queued_call:make([{project_id, Project}, {channel_id, Channel}, {call_flow_id, Flow}, {address, <<"123">>}]),
  QueuedCall2 = queued_call:make([{project_id, Project}, {channel_id, Channel}, {call_flow_id, Flow}, {address, <<"124">>}]),
  scheduler:load(),

  mock_broker:wait_dispatch(QueuedCall1#queued_call.id),
  mock_broker:wait_dispatch(QueuedCall2#queued_call.id),
  meck:unload(),
  ok.

unanswered_call() ->
  mock_broker:start(),

  Project = project:make(),
  Flow = call_flow:make([{project_id, Project}, {broker_flow, [answer, hangup]}]),
  Channel = channel:make([{call_flow_id, Flow}]),
  QueuedCall = queued_call:make([{project_id, Project}, {channel_id, Channel}, {call_flow_id, Flow}, {address, <<"123">>}]),
  scheduler:load(),

  SessionPid = mock_broker:wait_dispatch(QueuedCall#queued_call.id),
  session:reject(SessionPid, no_answer),

  ?assertEqual(normal, test_app:wait_process(SessionPid)),

  meck:unload().

unexpected_error() ->
  Flow = call_flow:make([{broker_flow, [answer, hangup]}]),
  Channel = channel:make([{call_flow_id, Flow}]),
  {ok, SessionPid} = session:new(),
  Pbx = pbx_mock:new([
    {answer, [], fun() -> error(unexpected_error) end}
  ]),

  session:answer(SessionPid, Pbx, Channel:id(), <<"1234">>),

  ?assertEqual(unexpected_error, test_app:wait_process(SessionPid)),
  ?assertEqual(ok, Pbx:validate()).
