-module(twilio_pbx_sup).
-export([start_link/0, start_session/1]).

-behaviour(supervisor).
-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, {}).

start_session(CallSid) ->
  supervisor:start_child(?SERVER, [CallSid]).

%% @private
init({}) ->
  PbxSpec = {null, {twilio_pbx, start_link, []},
                 temporary, 1000, worker, [twilio_pbx]},
  {ok, {{simple_one_for_one, 0, 1}, [PbxSpec]}}.
