-module(assign_expression_test).
-include_lib("eunit/include/eunit.hrl").
-include("session.hrl").
-include("db.hrl").

do_test() ->
  Session = #session{session_id = "1", call_log = #call_log{}, js_context = erjs_context:new()},
  {next, NewSession} = assign_expression:run([{name, "foo"}, {data, "1+1"}], Session),
  ?assertEqual([{<<"foo">>, 2}], erjs_context:to_list(NewSession#session.js_context)).
