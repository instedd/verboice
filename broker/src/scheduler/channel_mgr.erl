-module(channel_mgr).
-export([start_link/0]).

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-include("db.hrl").

-record(state, {channel_queues}).

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, {}, []).

%% @private
init({}) ->
  gen_server:cast(?SERVER, load_channels),
  {ok, #state{channel_queues = dict:new()}}.

%% @private
handle_call(_Request, _From, State) ->
  {reply, {error, unknown_call}, State}.

%% @private
handle_cast(load_channels, State = #state{channel_queues = Queues}) ->
  Channels = channel:find_all(),
  NewQueues = lists:foldl(fun (Channel = #channel{id = Id}, Q) ->
    ChannelSpec = {{channel, Id}, {channel_queue, start_link, [Channel]}, transient, 5000, worker, [channel_queue]},
    {ok, Pid} = supervisor:start_child(scheduler_sup, ChannelSpec),
    dict:store(Id, Pid, Q)
  end, Queues, Channels),
  {noreply, State#state{channel_queues = NewQueues}};

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