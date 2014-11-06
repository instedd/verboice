
-module(verboice_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

-ifndef(TEST).
pbx_supervisors() -> [
  ?CHILD(asterisk_sup, supervisor),
  ?CHILD(twilio_sup, supervisor)
].
-else.
pbx_supervisors() -> [].
-endif.



init([]) ->
  {ok, DbName} = application:get_env(db_name),
  {ok, DbUser} = application:get_env(db_user),
  {ok, DbPass} = application:get_env(db_pass),
  DbHost = case application:get_env(db_host) of
    {ok, Value} -> Value;
    undefined -> "localhost"
  end,

  {ok, { {one_for_one, 5, 10}, [
    {mysql, {mysql, start_link, [db, DbHost, undefined, DbUser, DbPass, DbName, fun log/4, utf8]},
      permanent, 5000, worker, [mysql]},
    ?CHILD(tz_server, worker),
    ?CHILD(cache, worker),
    ?CHILD(call_log_entry_srv, worker),
    ?CHILD(session_sup, supervisor),
    ?CHILD(scheduler_sup, supervisor)
    | pbx_supervisors()
  ]} }.

log(_Module, _Line, debug, _FormatFun) -> ok;
log(Module, Line, Level, FormatFun) ->
  {Format, Arguments} = FormatFun(),
  LagerLevel = case Level of
    normal -> info;
    X -> X
  end,
  lager:log(LagerLevel, self(), "~w:~b: "++ Format ++ "~n", [Module, Line] ++ Arguments).
