-module(channel_mgr).
-export([start_link/0]).

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-include("db.hrl").

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, {}, []).

%% @private
init({}) ->
  gen_server:cast(?SERVER, load_channels),
  {ok, undefined}.

%% @private
handle_call(_Request, _From, State) ->
  {reply, {error, unknown_call}, State}.

%% @private
handle_cast(load_channels, State) ->
  Channels = channel:find_all(),
  lists:foreach(fun (Channel = #channel{id = Id}) ->
    ChannelSpec = {{channel, Id}, {channel_queue, start_link, [Channel]}, transient, 5000, worker, [channel_queue]},
    supervisor:start_child(scheduler_sup, ChannelSpec)
  end, Channels),
  {noreply, State};

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