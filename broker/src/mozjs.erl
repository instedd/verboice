-module(mozjs).
-export([new_runtime/0, new_context/1, shutdown/0, eval/2]).
-on_load(init/0).

init() ->
  Ebin = filename:dirname(code:which(?MODULE)),
  File = filename:join(filename:dirname(Ebin), "priv/verboice_drv"),
  ok = erlang:load_nif(File, 0).

new_runtime() ->
  exit(nif_library_not_loaded).

new_context(_Runtime) ->
  exit(nif_library_not_loaded).

shutdown() ->
  exit(nif_library_not_loaded).

eval(_Context, _Script) ->
  exit(nif_library_not_loaded).