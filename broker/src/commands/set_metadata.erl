-module(set_metadata).
-export([run/2]).
-include("session.hrl").
-include_lib("poirot/include/poirot.hrl").

run(Args, Session = #session{js_context = JS}) ->
  Metadata = prepare_metadata(Args, JS),
  poirot:add_meta(util:to_poirot(Metadata)),
  {next, Session}.

prepare_metadata([], _) -> [];
prepare_metadata([{Key, [eval, Expr]} | T], JS) ->
  {Value, _} = erjs:eval(Expr, JS),
  [{Key, Value} | prepare_metadata(T, JS)];
prepare_metadata([{Key, Value} | T], JS) ->
  [{Key, Value} | prepare_metadata(T, JS)].
