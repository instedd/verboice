-module(js_test).
-include_lib("eunit/include/eunit.hrl").
-include("session.hrl").
-include("db.hrl").

do_test() ->
  Session = #session{session_id = "1", call_log = #call_log{}, js_context = erjs_context:new()},
  {next, NewSession} = js:run([{source, "x = 1"}], Session),
  ?assertEqual([{<<"x">>, 1}], erjs_context:to_list(NewSession#session.js_context)).
