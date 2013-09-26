-module(twiml_test).
-include_lib("eunit/include/eunit.hrl").

parse_cases() -> [
  {?LINE, "<Response><Play>http://foo</Play></Response>", [[play_url, [{url, "http://foo"}]]]},
  {?LINE, "<Response><Say>Hello</Say></Response>", [[say, [{text, "Hello"}]]]},
  {?LINE, "<Response><Hangup/></Response>", [hangup]},
  {?LINE, "<Response>\n<Hangup/>\n</Response>", [hangup]},
  {?LINE, "<Response><Pause /></Response>", [pause]},
  {?LINE, "<Response><Pause length=\"3\"/></Response>", [[pause, [{length, 3}]]]},
  {?LINE, "<Response><Gather/></Response>", gather_commands([{min,1}, {max,infinity}])},
  {?LINE, "<Response><Gather timeout=\"3\" finishOnKey=\"*\" numDigits=\"4\"/></Response>",
    gather_commands([{min, 4}, {max, 4}, {finish_on_key, "*"}, {timeout, 3}])},
  {?LINE, "<Response><Gather><Play>http://foo</Play></Gather></Response>",
    gather_commands([{play, "http://foo"}, {min, 1}, {max, infinity}])},
  {?LINE, "<Response><Gather><Say>hello</Say></Gather></Response>",
    gather_commands([{say, "hello"}, {min, 1}, {max, infinity}])},
  {?LINE, "<Response><Gather>\n<Say>hello</Say>\n</Gather></Response>",
    gather_commands([{say, "hello"}, {min, 1}, {max, infinity}])},
  {?LINE, "<Response><Gather/><Hangup /></Response>",
    gather_commands([{min, 1}, {max, infinity}], [hangup])},
  {?LINE, "<Response><Gather action=\"http://foo.com\" method=\"GET\"/></Response>",
    gather_commands([{min, 1}, {max, infinity}], [], [{method, get}, {url, "http://foo.com"}])},

  {?LINE, "<Response><Redirect>http://foo.com</Redirect></Response>",
    [[callback, [{url, "http://foo.com"}]]]},
  {?LINE, "<Response><Redirect method=\"get\">http://foo.com</Redirect></Response>",
    [[callback, [{url, "http://foo.com"}, {method, get}]]]}
].

gather_commands(CaptureOptions) -> gather_commands(CaptureOptions, []).
gather_commands(CaptureOptions, NextCommands) -> gather_commands(CaptureOptions, NextCommands, []).
gather_commands(CaptureOptions, NextCommands, CallbackOptions) ->
  NextIndex = case NextCommands of [] -> 3; _ -> 4 end,
  [
    [capture, CaptureOptions],
    ['if', [{condition, "timeout || finish_key"}, {then, NextIndex}]],
    [callback, CallbackOptions ++ [{params, [{"Digits", "digits"}]}]],
    stop
  ] ++ NextCommands.

parse_test_() ->
  [{Line, ?_assertEqual(Commands, twiml:parse(Xml))} || {Line, Xml, Commands} <- parse_cases()].
