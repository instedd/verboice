-module(scheduler_sup).
-export([start_link/0]).

-behaviour(supervisor).
-export([init/1]).

-define(SERVER, ?MODULE).
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, {}).

%% @private
init({}) ->
  Children = [
    ?CHILD(channel_mgr, worker),
    ?CHILD(scheduler, worker),
    ?CHILD(channel_sup, supervisor)
  ],
  RestartStrategy = {one_for_all, 5, 10},
  {ok, {RestartStrategy, Children}}.
