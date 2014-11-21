-module(twiml_test).
-include_lib("eunit/include/eunit.hrl").

parse_cases() -> [
  {?LINE, "<Response><Play>http://foo</Play></Response>",
    [start_activity("<Play>http://foo</Play>", "twiml_play"), [play_url, [{url, "http://foo"}]]]},
  {?LINE, "<Response><Play>http://foo?bar&amp;baz</Play></Response>",
    [start_activity("<Play>http://foo?bar&amp;baz</Play>", "twiml_play"), [play_url, [{url, "http://foo?bar&baz"}]]]},
  {?LINE, "<Response><Say>Hello &amp; Bye</Say></Response>",
    [start_activity("<Say>Hello &amp; Bye</Say>", "twiml_say"), [say, [{text, "Hello & Bye"}]]]},
  {?LINE, "<Response><Say language=\"en\">Hello</Say></Response>",
    [start_activity("<Say language=\"en\">Hello</Say>", "twiml_say"), [say, [{text, "Hello"}, {language, "en"}]]]},
  {?LINE, "<Response><Hangup/></Response>", [start_activity("<Hangup/>", "twiml_hangup"), hangup]},
  {?LINE, "<Response>\n<Hangup/>\n</Response>", [start_activity("<Hangup/>", "twiml_hangup"), hangup]},
  {?LINE, "<Response><Pause /></Response>", [start_activity("<Pause/>", "twiml_pause"), pause]},
  {?LINE, "<Response><Pause length=\"3\"/></Response>", [start_activity("<Pause length=\"3\"/>", "twiml_pause"), [pause, [{length, 3}]]]},
  {?LINE, "<Response><Gather/></Response>", gather_commands("<Gather/>", [{min,1}, {max,infinity}])},
  {?LINE, "<Response><Gather timeout=\"3\" finishOnKey=\"*\" numDigits=\"4\"/></Response>",
    gather_commands("<Gather timeout=\"3\" finishOnKey=\"*\" numDigits=\"4\"/>", [{min, 4}, {max, 4}, {finish_on_key, "*"}, {timeout, 3}])},
  {?LINE, "<Response><Gather><Play>http://foo</Play></Gather></Response>",
    gather_commands("<Gather><Play>http://foo</Play></Gather>", [{play, "http://foo"}, {min, 1}, {max, infinity}])},
  {?LINE, "<Response><Gather><Say>hello</Say></Gather></Response>",
    gather_commands("<Gather><Say>hello</Say></Gather>", [{say, "hello"}, {min, 1}, {max, infinity}])},
  {?LINE, "<Response><Gather>\n<Say>hello</Say>\n</Gather></Response>",
    gather_commands("<Gather>\n<Say>hello</Say>\n</Gather>", [{say, "hello"}, {min, 1}, {max, infinity}])},
  {?LINE, "<Response><Gather/><Hangup /></Response>",
    gather_commands("<Gather/>", [{min, 1}, {max, infinity}], [start_activity("<Hangup/>", "twiml_hangup"), hangup])},
  {?LINE, "<Response><Gather action=\"http://foo.com\" method=\"GET\"/></Response>",
    gather_commands("<Gather action=\"http://foo.com\" method=\"GET\"/>", [{min, 1}, {max, infinity}], [], [{method, get}, {url, "http://foo.com"}])},

  {?LINE, "<Response><Redirect>http://foo.com</Redirect></Response>",
    [start_activity("<Redirect>http://foo.com</Redirect>", "twiml_redirect"), [callback, [{url, "http://foo.com"}]]]},
  {?LINE, "<Response><Redirect method=\"get\">http://foo.com</Redirect></Response>",
    [start_activity("<Redirect method=\"get\">http://foo.com</Redirect>", "twiml_redirect"), [callback, [{url, "http://foo.com"}, {method, get}]]]}
].

gather_commands(Name, CaptureOptions) -> gather_commands(Name, CaptureOptions, []).
gather_commands(Name, CaptureOptions, NextCommands) -> gather_commands(Name, CaptureOptions, NextCommands, []).
gather_commands(Name, CaptureOptions, NextCommands, CallbackOptions) ->
  NextIndex = case NextCommands of [] -> 5; _ -> 6 end,
  [
    start_activity(Name, "twiml_gather"),
    [capture, CaptureOptions],
    ['if', [{condition, "timeout || finish_key"}, {then, NextIndex}]],
    set_metadata("pressed", [eval, "digits"]),
    [callback, CallbackOptions ++ [{params, [{"Digits", "digits"}]}]],
    stop,
    set_metadata("timeout")
  ] ++ NextCommands.

parse_test_() ->
  [{Line, ?_assertEqual(begin io:format("~p~n~p~n", [Commands, twiml:parse(Xml)]), Commands end, twiml:parse(Xml))} || {Line, Xml, Commands} <- parse_cases()].

start_activity(Name, StepType) ->
  [start_activity, [{name, iolist_to_binary(Name)}, {metadata, [{step_type, StepType}]}]].

set_metadata(Result, Data) ->
  [set_metadata, [{step_result, Result}, {step_data, Data}]].

set_metadata(Result) ->
  [set_metadata, [{step_result, Result}]].

parse_record_test_() ->
  [{?LINE, ?_assertMatch(
    [[start_activity, [{name, <<"<Record/>">>}, {metadata, [{step_type, "twiml_record"}]}]],
     [record, [{key, _}, {description, _}]],
     [callback, [{params, [{"RecordingUrl", "record_url" ++ _}]}]]],
    twiml:parse("<Response><Record/></Response>"))},
   {?LINE, ?_assertMatch(
    [[start_activity, [{name, <<"<Record timeout=\"5\" finishOnKey=\"*\"/>">>}, {metadata, [{step_type, "twiml_record"}]}]],
     [record, [{key, _}, {description, _}, {stop_keys, "*"}, {timeout, 5}]],
     [callback, [{params, [{"RecordingUrl", "record_url" ++ _}]}]]],
    twiml:parse("<Response><Record timeout=\"5\" finishOnKey=\"*\"/></Response>"))},
   {?LINE, ?_assertMatch(
    [[start_activity, [{name, <<"<Record action=\"http://foo\" method=\"get\"/>">>}, {metadata, [{step_type, "twiml_record"}]}]],
     [record, [{key, _}, {description, _}]],
     [callback, [{method, get}, {url, "http://foo"}, {params, [{"RecordingUrl", "record_url" ++ _}]}]]],
    twiml:parse("<Response><Record action=\"http://foo\" method=\"get\"/></Response>"))}].

