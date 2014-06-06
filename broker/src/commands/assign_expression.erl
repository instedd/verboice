-module(assign_expression).
-export([run/2]).
-include("session.hrl").

run(Args, Session = #session{js_context = JS}) ->
  Name = proplists:get_value(name, Args),
  Data = proplists:get_value(data, Args),
  {Value, _} = erjs:eval(Data, JS),
  poirot:log(debug, "Assign ~s = ~s (= ~p)", [Name, Data, Value]),
  NewSession = Session#session{js_context = erjs_context:set(list_to_atom(Name), Value, JS)},
  {next, NewSession}.
