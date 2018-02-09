-module(asterisk_sup).
-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).
-define(CHILD(I, Type, Options), {I, {I, start_link, [Options]}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    AmiHost = verboice_config:ami_host(),
    AmiPort = verboice_config:ami_port(),
    {ok, { {one_for_all, 5, 10}, [
      ?CHILD(ami_client, worker, [{ami_host, AmiHost}, {ami_port, AmiPort}]),
      ?CHILD(ami_events, worker),
      ?CHILD(agi_session_sup, supervisor),
      ?CHILD(agi_events, worker),
      ?CHILD(agi_server, worker),
      ?CHILD(asterisk_channel_srv, worker),
      ?CHILD(asterisk_broker, worker)
    ]} }.
