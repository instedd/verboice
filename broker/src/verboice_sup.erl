
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
      ?CHILD(asterisk_sup, supervisor),
      ?CHILD(session_sup, supervisor),
      {mysql, {mysql, start_link, [db, "localhost", "root", "", "verboice_development"]}, permanent, 5000, worker, [mysql]}
    ]} }.

