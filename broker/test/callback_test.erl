-module(callback_test).
-include_lib("eunit/include/eunit.hrl").
-include("session.hrl").
-include("db.hrl").

make_session() ->
  meck:new(call_log_srv, [stub_all]),
  meck:expect(call_log_srv, id, 1, 1),
  #session{session_id = "1", call_log = {call_log_srv}, project = #project{id = 42}}.

get_to_url_in_param_test() ->
  Session = make_session(),
  meck:new(httpc),

  meck:expect(httpc, request, [get, {"http://foo.com/?CallSid=1", []}, [], []], {ok, {"200 OK", [], "<Response/>"}}),

  {{exec, []}, _} = callback:run([{method, "get"}, {url, "http://foo.com"}], Session),
  meck:unload().

post_to_url_in_param_test() ->
  Session = make_session(),
  meck:new(httpc),

  meck:expect(httpc, request, [post, {"http://foo.com/", [], "application/x-www-form-urlencoded", "CallSid=1"}, [], []],
    {ok, {"200 OK", [], "<Response/>"}}),

  {{exec, []}, _} = callback:run([{url, "http://foo.com"}], Session),
  meck:unload().


get_to_url_in_session_test() ->
  Session = (make_session())#session{call_flow = #call_flow{callback_url = <<"http://foo.com">>}},
  meck:new(httpc),

  meck:expect(httpc, request, [get, {"http://foo.com/?CallSid=1", []}, [], []], {ok, {"200 OK", [], "<Response/>"}}),

  {{exec, []}, _} = callback:run([{method, "get"}], Session),
  meck:unload().

get_to_url_with_session_callback_params_test() ->
  Session = (make_session())#session{callback_params = [{"foo", "1"}]},
  meck:new(httpc),

  meck:expect(httpc, request, [get, {"http://foo.com/?CallSid=1&foo=1", []}, [], []], {ok, {"200 OK", [], "<Response/>"}}),

  {{exec, []}, _} = callback:run([{method, "get"}, {url, "http://foo.com"}], Session),
  meck:unload().

handle_response_with_variables_test() ->
  Session = (make_session())#session{js_context = erjs_context:new()},
  meck:new(httpc),

  meck:expect(httpc, request, 4, {ok, {"200 OK", [], "{\"foo\":1, \"bar\":\"baz\"}"}}),

  {next, #session{js_context = JS}} = callback:run([{url, "http://foo.com"}, {response_type, variables}], Session),
  ?assertEqual(1, erjs_context:get(response_foo, JS)),
  ?assertEqual("baz", erjs_context:get(response_bar, JS)),

  meck:unload().

callback_without_response_test() ->
  Session = make_session(),
  meck:new(httpc),

  meck:expect(httpc, request, 4, {ok, {"200 OK", [], ""}}),

  {next, Session} = callback:run([{url, "http://foo.com"}, {response_type, none}], Session),
  meck:unload().

include_params_test() ->
  Session = (make_session())#session{js_context = erjs_context:new([{var_foo, 1}])},
  meck:new(httpc),

  meck:expect(httpc, request, [get, {"http://foo.com/?foo=1&CallSid=1", []}, [], []], {ok, {"200 OK", [], "<Response/>"}}),

  {{exec, []}, _} = callback:run([{url, "http://foo.com"}, {method, "get"}, {params, [{"foo", "var_foo"}]}], Session),
  meck:unload().

include_object_params_test() ->
  {_, Context} = erjs:eval("var_foo = {}; var_foo['bar'] = 1"),
  Session = (make_session())#session{js_context = Context},
  meck:new(httpc),

  meck:expect(httpc, request, [get, {"http://foo.com/?foo%5Bbar%5D=1&CallSid=1", []}, [], []], {ok, {"200 OK", [], "<Response/>"}}),

  {{exec, []}, _} = callback:run([{url, "http://foo.com"}, {method, "get"}, {params, [{"foo", "var_foo"}]}], Session),
  meck:unload().

include_nested_object_params_test() ->
  {_, Context} = erjs:eval("var_foo = {}; var_foo['bar'] = {}; var_foo['bar']['baz'] = 2"),
  Session = (make_session())#session{js_context = Context},
  meck:new(httpc),

  meck:expect(httpc, request, [get, {"http://foo.com/?foo%5Bbar%5D%5Bbaz%5D=2&CallSid=1", []}, [], []], {ok, {"200 OK", [], "<Response/>"}}),

  {{exec, []}, _} = callback:run([{url, "http://foo.com"}, {method, "get"}, {params, [{"foo", "var_foo"}]}], Session),
  meck:unload().


async_callback_test() ->
  Session = make_session(),
  meck:new(delayed_job),

  meck:expect(delayed_job, enqueue, 1, ok),

  {next, Session} = callback:run([{url, "http://foo.com"}, {async, true}], Session),
  Task = iolist_to_binary(meck:capture(last, delayed_job, enqueue, 1, 1)),

  % Make sure the object is properly tagged
  <<"--- !ruby/object:Jobs::CallbackJob\n", _/binary>> = Task,

  {ok, [Job]} = yaml:load(Task, [{schema, yaml_schema_ruby}]),
  ?assertEqual("post", proplists:get_value(method, Job)),
  ?assertEqual([{"CallSid", 1}], proplists:get_value(body, Job)),
  ?assertEqual("http://foo.com", proplists:get_value(url, Job)),

  meck:unload().
