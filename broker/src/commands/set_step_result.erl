-module(set_step_result).
-export([run/2]).
-include("session.hrl").
-include_lib("poirot_erlang/include/poirot.hrl").

run(Args, Session = #session{js_context = JS}) ->
  Result = proplists:get_value(result, Args),
  poirot:add_meta([{step_result, Result}]),

  case proplists:get_value(data, Args) of
    undefined -> ok;
    DataExpr ->
      Value = case erjs:eval(DataExpr, JS) of
        {Str, _} when is_list(Str) -> list_to_binary(Str);
        {X, _} -> X
      end,
      poirot:add_meta([{step_data, Value}])
  end,

  {next, Session}.
