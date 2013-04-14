-module(mozjs).
-export([new_runtime/0, new_context/1, shutdown/0, eval/2]).
-on_load(init/0).

init() ->
  Ebin = filename:dirname(code:which(?MODULE)),
  File = filename:join(filename:dirname(Ebin), "priv/verboice_drv"),
  ok = erlang:load_nif(File, 0).

-spec new_runtime() -> port().
new_runtime() ->
  erlang:nif_error(module_not_loaded, []).

-spec new_context(port()) -> port().
new_context(Runtime) ->
  erlang:nif_error(module_not_loaded, [Runtime]).

shutdown() ->
  erlang:nif_error(module_not_loaded, []).

-spec eval(port(), string()) -> term().
eval(Context, Script) ->
  erlang:nif_error(module_not_loaded, [Context, Script]).