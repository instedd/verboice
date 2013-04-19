-module(broker).
-export([start_link/1, dispatch/2]).

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-record(state, {real_broker, ready_channels}).

-include("session.hrl").
-include("db.hrl").

start_link(RealBroker) ->
  gen_server:start_link({local, RealBroker}, ?MODULE, RealBroker, []).

%% @private
init(RealBroker) ->
  {ok, #state{real_broker = RealBroker, ready_channels = sets:new()}}.

dispatch(Channel, QueuedCall) ->
  Broker = channel:broker(Channel),
  gen_server:call(Broker, {dispatch, Channel, QueuedCall}).

%% @private
handle_call({dispatch, Channel, QueuedCall}, _From, State = #state{real_broker = RealBroker}) ->
  {ok, SessionId} = session:new(),
  session:dial(SessionId, RealBroker, Channel, QueuedCall),
  {reply, ok, State};

handle_call(_Request, _From, State) ->
  {reply, {error, unknown_call}, State}.

%% @private
% handle_cast({notify, Queue}, State = #state{ready_channels = ReadyChannels}) ->
%   channel_queue:dispatch(Queue),
%   {noreply, State#state{ready_channels = sets:add_element(Queue, ReadyChannels)}};

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