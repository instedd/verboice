-module(callback_test).
-include_lib("eunit/include/eunit.hrl").
-include("session.hrl").
-include("db.hrl").

get_to_url_in_param_test() ->
  Session = #session{session_id = "1", call_log = #call_log{}},
  meck:new(call_log, [stub_all]),
  meck:new(httpc),

  meck:expect(httpc, request, [get, {"http://foo.com/?CallSid=1", []}, [], []], {ok, {"200 OK", [], "<Response/>"}}),

  {{exec, []}, _} = callback:run([{method, "get"}, {url, "http://foo.com"}], Session),
  meck:unload().

post_to_url_in_param_test() ->
  Session = #session{session_id = "1", call_log = #call_log{}},
  meck:new(call_log, [stub_all]),
  meck:new(httpc),

  meck:expect(httpc, request, [post, {"http://foo.com/", [], "application/x-www-form-urlencoded", "CallSid=1"}, [], []],
    {ok, {"200 OK", [], "<Response/>"}}),

  {{exec, []}, _} = callback:run([{url, "http://foo.com"}], Session),
  meck:unload().


get_to_url_in_session_test() ->
  Session = #session{session_id = "1", call_log = #call_log{}, call_flow = #call_flow{callback_url = <<"http://foo.com">>}},
  meck:new(call_log, [stub_all]),
  meck:new(httpc),

  meck:expect(httpc, request, [get, {"http://foo.com/?CallSid=1", []}, [], []], {ok, {"200 OK", [], "<Response/>"}}),

  {{exec, []}, _} = callback:run([{method, "get"}], Session),
  meck:unload().

get_to_url_with_session_callback_params_test() ->
  Session = #session{session_id = "1", call_log = #call_log{}, callback_params = [{"foo", "1"}]},
  meck:new(call_log, [stub_all]),
  meck:new(httpc),

  meck:expect(httpc, request, [get, {"http://foo.com/?CallSid=1&foo=1", []}, [], []], {ok, {"200 OK", [], "<Response/>"}}),

  {{exec, []}, _} = callback:run([{method, "get"}, {url, "http://foo.com"}], Session),
  meck:unload().

