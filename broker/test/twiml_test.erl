-module(twiml_test).
-include_lib("eunit/include/eunit.hrl").

parse_cases() -> [
  {"<Response><Play>http://foo</Play></Response>", [[play_url, [{url, "http://foo"}]]]},
  {"<Response><Say>Hello</Say></Response>", [[say, [{text, "Hello"}]]]},
  {"<Response><Hangup/></Response>", [hangup]},
  {"<Response><Pause /></Response>", [pause]},
  {"<Response><Pause length=\"3\"/></Response>", [[pause, [{length, 3}]]]},
  {"<Response><Gather/></Response>", gather_commands([{min,1}, {max,infinity}])},
  {"<Response><Gather timeout=\"3\" finishOnKey=\"*\" numDigits=\"4\"/></Response>",
    gather_commands([{min, 4}, {max, 4}, {finish_on_key, "*"}, {timeout, 3}])},
  {"<Response><Gather><Play>http://foo</Play></Gather></Response>",
    gather_commands([{play, "http://foo"}, {min, 1}, {max, infinity}])},
  {"<Response><Gather><Say>hello</Say></Gather></Response>",
    gather_commands([{say, "hello"}, {min, 1}, {max, infinity}])},
  {"<Response><Gather/><Hangup /></Response>",
    gather_commands([{min, 1}, {max, infinity}], [hangup])},
  {"<Response><Gather action=\"http://foo.com\" method=\"GET\"/></Response>",
    gather_commands([{min, 1}, {max, infinity}], [stop], [{method, get}, {url, "http://foo.com"}])},

  {"<Response><Redirect>http://foo.com</Redirect></Response>",
    [[callback, [{url, "http://foo.com"}]]]},
  {"<Response><Redirect method=\"get\">http://foo.com</Redirect></Response>",
    [[callback, [{url, "http://foo.com"}, {method, get}]]]}
].

gather_commands(CaptureOptions) -> gather_commands(CaptureOptions, [stop]).
gather_commands(CaptureOptions, NextCommands) -> gather_commands(CaptureOptions, NextCommands, []).
gather_commands(CaptureOptions, NextCommands, CallbackOptions) ->
  [
    [capture, CaptureOptions],
    ['if', [{condition, "timeout || finish_key"}, {then, 3}]],
    [callback, CallbackOptions ++ [{params, [{"Digits", "digits"}]}]]
  ] ++ NextCommands.

parse_test_() ->
  [?_assertEqual(Commands, twiml:parse(Xml)) || {Xml, Commands} <- parse_cases()].