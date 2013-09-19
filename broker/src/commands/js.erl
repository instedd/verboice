-module(js).
-export([run/2]).
-include("session.hrl").

run(Args, Session = #session{js_context = JS}) ->
  Source = proplists:get_value(source, Args),
  {_, JS2} = erjs:eval(Source, JS),
  {next, Session#session{js_context = JS2}}.
