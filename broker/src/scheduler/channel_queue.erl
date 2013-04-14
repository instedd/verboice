-module(channel_queue).
-export([start_link/1, enqueue/1]).

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-include("db.hrl").

-record(state, {channel, queued_calls}).

start_link(Channel = #channel{id = Id}) ->
  gen_server:start_link({global, {channel, Id}}, ?MODULE, Channel, []).

enqueue(QueuedCall = #queued_call{channel_id = ChannelId}) ->
  gen_server:cast({global, {channel, ChannelId}}, {enqueue, QueuedCall}).

%% @private
init(Channel) ->
  {ok, #state{channel = Channel, queued_calls = queue:new()}}.

%% @private
handle_call(_Request, _From, State) ->
  {reply, {error, unknown_call}, State}.

%% @private
handle_cast({enqueue, QueuedCall}, State = #state{queued_calls = Queue}) ->
  {noreply, State#state{queued_calls = queue:in(QueuedCall, Queue)}};

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
