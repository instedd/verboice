-module(channel_sup).
-export([start_link/0]).

-behaviour(supervisor).
-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, {}).

init({}) ->
  ChannelSpec = {null, {channel_queue, start_link, []},
                 temporary, 1000, worker, [channel_queue]},
  {ok, {{simple_one_for_one, 0, 1}, [ChannelSpec]}}.
