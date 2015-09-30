-module(main).
-export([start/0]).
-export([ensure_all_started/1, ensure_all_started/2]).

start() ->
  application:load(verboice),
  case sanity_check:check_all() of
    {error, Reason} ->
      error({"Startup check failed", Reason});
    ok ->
      ensure_all_started(verboice, permanent),
      ensure_all_started(ernie_server, permanent)
  end.


ensure_all_started(Application) ->
    ensure_all_started(Application, temporary).

ensure_all_started(Application, Type) ->
    case ensure_all_started(Application, Type, []) of
  {ok, Started} ->
      {ok, lists:reverse(Started)};
  {error, Reason, Started} ->
      _ = [application:stop(App) || App <- Started],
      {error, Reason}
    end.

ensure_all_started(Application, Type, Started) ->
    case application:start(Application, Type) of
  ok ->
      {ok, [Application | Started]};
  {error, {already_started, Application}} ->
      {ok, Started};
  {error, {not_started, Dependency}} ->
      case ensure_all_started(Dependency, Type, Started) of
    {ok, NewStarted} ->
        ensure_all_started(Application, Type, NewStarted);
    Error ->
        Error
      end;
  {error, Reason} ->
      {error, {Application, Reason}, Started}
    end.
