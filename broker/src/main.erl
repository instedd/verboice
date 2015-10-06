-module(main).
-export([start/0]).

start() ->
  application:load(verboice),
  case sanity_check:check_all() of
    {error, Reason} ->
      error({"Startup check failed", Reason});
    ok ->
      application:ensure_all_started(verboice, permanent),
      application:ensure_all_started(ernie_server, permanent)
  end.
