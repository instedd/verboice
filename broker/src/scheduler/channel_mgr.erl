-module(channel_mgr).
-export([start_link/0, ensure_channel/1]).

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-include("db.hrl").

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, {}, []).

ensure_channel(ChannelId) ->
  gen_server:call(?SERVER, {ensure_channel, ChannelId}).

%% @private
init({}) ->
  {ok, undefined}.

%% @private
handle_call({ensure_channel, Id}, _From, State) ->
  case channel_queue:whereis_channel(Id) of
    undefined ->
      Channel = channel:find(Id),
      {ok, _Pid} = supervisor:start_child(channel_sup, [Channel]);
    _ -> ok
  end,
  {reply, ok, State};

handle_call(_Request, _From, State) ->
  {reply, {error, unknown_call}, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

%% @private
handle_info(_Info, State) ->
  {noreply, State}.

%% @private
terminate(_Reason, _State) ->
  ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
