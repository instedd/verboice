-module(session_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, count/1]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

count(Criteria) ->
  count(supervisor:which_children(?MODULE), Criteria, 0).

count([], _, Count) -> Count;
count([{_, Pid, _, _} | Rest], Criteria, Count) when is_pid(Pid) ->
  NewCount = Count + case session:matches(Pid, Criteria) of true -> 1; _ -> 0 end,
  count(Rest, Criteria, NewCount);
count([_ | Rest], Criteria, Count) ->
  count(Rest, Criteria, Count).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    {ok, { {one_for_one, 5, 10}, []} }.
