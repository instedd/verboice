-module(assign_value).
-export([run/2]).
-include("session.hrl").

run(Args, Session = #session{js_context = JS}) ->
  Name = proplists:get_value(name, Args),
  Data = proplists:get_value(data, Args),
  NewSession = Session#session{js_context = erjs_context:set(list_to_atom(Name), Data, JS)},
  {next, NewSession}.
