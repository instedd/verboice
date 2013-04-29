
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

init([]) ->
  {ok, { {one_for_one, 5, 10}, [
    {mysql, {mysql, start_link, [db, "localhost", undefined, "root", "", "verboice_development", fun log/4, utf8]},
      permanent, 5000, worker, [mysql]},
    ?CHILD(tz_server, worker),
    ?CHILD(asterisk_sup, supervisor),
    ?CHILD(session_sup, supervisor),
    ?CHILD(scheduler_sup, supervisor)
  ]} }.

log(Module, Line, Level, FormatFun) ->
  {Format, Arguments} = FormatFun(),
  lager:log(Level, self(), "~w:~b: "++ Format ++ "~n", [Module, Line] ++ Arguments).
