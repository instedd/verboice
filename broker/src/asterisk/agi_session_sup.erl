-module(agi_session_sup).
-export([start_link/0, start_session/1]).

-behaviour(supervisor).
-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, {}).

start_session(Sock) ->
  case supervisor:start_child(?SERVER, [Sock]) of
    {error, Error} -> exit(Error);
    Return = {ok, Pid} ->
      gen_tcp:controlling_process(Sock, Pid),
      Return
  end.

%% @private
init({}) ->
  ChannelSpec = {null, {agi_session, start_link, []},
                 temporary, 1000, worker, [agi_channel]},
  {ok, {{simple_one_for_one, 0, 1}, [ChannelSpec]}}.