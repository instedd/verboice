-module(say_test).
-include_lib("eunit/include/eunit.hrl").
-include("session.hrl").

do_test() ->
  Session = #session{pbx = pbx, default_language = "en", js_context = erjs_context:new([{var_name, "World"}])},
  meck:new(pbx, [non_strict]),
  meck:expect(pbx, can_play, 1, true),
  meck:expect(pbx, play, 1, ok),
  {next, Session} = say:run([{text, "Hello {name}"}], Session),

  ?assert(meck:called(pbx, play, [{text, "en", <<"Hello World">>}])),
  meck:unload().
