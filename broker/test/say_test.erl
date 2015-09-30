-module(say_test).
-include_lib("eunit/include/eunit.hrl").
-include("session.hrl").

do_test() ->
  Context = session:create_default_erjs_context(1, "44445555"),
  Session = #session{pbx = pbx, default_language = "en", js_context = erjs_context:set(var_name, "World", Context)},
  meck:new(pbx, [non_strict]),
  meck:expect(pbx, can_play, 1, true),
  meck:expect(pbx, play, 1, ok),
  {next, Session} = say:run([{text, "Hello {name}"}], Session),

  ?assert(meck:called(pbx, play, [{text, "en", <<"Hello World">>}])),
  meck:unload().

number_test() ->
  Context = session:create_default_erjs_context(1, "44445555"),
  Session = #session{pbx = pbx, default_language = "en", js_context = erjs_context:set(var_foo, "12345", Context)},
  meck:new(pbx, [non_strict]),
  meck:expect(pbx, can_play, 1, true),
  meck:expect(pbx, play, 1, ok),
  {next, Session} = say:run([{text, "Hello {foo}"}], Session),

  ?assert(meck:called(pbx, play, [{text, "en", <<"Hello 12345">>}])),
  meck:unload().

integer_test() ->
  Context = session:create_default_erjs_context(1, "44445555"),
  Session = #session{pbx = pbx, default_language = "en", js_context = erjs_context:set(var_foo, 12345, Context)},
  meck:new(pbx, [non_strict]),
  meck:expect(pbx, can_play, 1, true),
  meck:expect(pbx, play, 1, ok),
  {next, Session} = say:run([{text, "Hello {foo}"}], Session),

  ?assert(meck:called(pbx, play, [{text, "en", <<"Hello 12345">>}])),
  meck:unload().

undefined_variable_test() ->
  Context = session:create_default_erjs_context(1, "44445555"),
  Session = #session{pbx = pbx, default_language = "en", js_context = Context},
  meck:new(pbx, [non_strict]),
  meck:expect(pbx, can_play, 1, true),
  meck:expect(pbx, play, 1, ok),
  {next, Session} = say:run([{text, "Hello {foo}"}], Session),

  ?assert(meck:called(pbx, play, [{text, "en", <<"Hello ">>}])),
  meck:unload().

say_digits_test() ->
  Context = session:create_default_erjs_context(1, "44445555"),
  Session = #session{pbx = pbx, default_language = "en", js_context = erjs_context:set(var_ssn, "123456", Context)},
  meck:new(pbx, [non_strict]),
  meck:expect(pbx, can_play, 1, true),
  meck:expect(pbx, play, 1, ok),
  {next, Session} = say:run([{text, "Your Social Security Number is{split_digits(ssn)}"}], Session),

  ?assert(meck:called(pbx, play, [{text, "en", <<"Your Social Security Number is 1 2 3 4 5 6">>}])),
  meck:unload().

say_digits_with_integer_test() ->
  Context = session:create_default_erjs_context(1, "44445555"),
  Session = #session{pbx = pbx, default_language = "en", js_context = erjs_context:set(var_ssn, 123456, Context)},
  meck:new(pbx, [non_strict]),
  meck:expect(pbx, can_play, 1, true),
  meck:expect(pbx, play, 1, ok),
  {next, Session} = say:run([{text, "Your Social Security Number is{split_digits(ssn)}"}], Session),

  ?assert(meck:called(pbx, play, [{text, "en", <<"Your Social Security Number is 1 2 3 4 5 6">>}])),
  meck:unload().

say_digits_with_undefined_test() ->
  Context = session:create_default_erjs_context(1, "44445555"),
  Session = #session{pbx = pbx, default_language = "en", js_context = Context},
  meck:new(pbx, [non_strict]),
  meck:expect(pbx, can_play, 1, true),
  meck:expect(pbx, play, 1, ok),
  {next, Session} = say:run([{text, "Your Social Security Number is{split_digits(ssn)}"}], Session),

  ?assert(meck:called(pbx, play, [{text, "en", <<"Your Social Security Number is">>}])),
  meck:unload().

say_phone_number_test() ->
  Context = session:create_default_erjs_context(1, "44445555"),
  Session = #session{pbx = pbx, default_language = "en", js_context = Context},
  meck:new(pbx, [non_strict]),
  meck:expect(pbx, can_play, 1, true),
  meck:expect(pbx, play, 1, ok),
  {next, Session} = say:run([{text, "Your phone number is {phone_number}"}], Session),

  ?assert(meck:called(pbx, play, [{text, "en", <<"Your phone number is 44445555">>}])),
  meck:unload().
