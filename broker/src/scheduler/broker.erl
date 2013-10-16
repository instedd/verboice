-module(broker).
-export([start_link/1, dispatch/2, notify_ready/1, behaviour_info/1]).

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-record(state, {real_broker, real_broker_state, ready_channels}).

-include("session.hrl").
-include("db.hrl").

start_link(RealBroker) ->
  gen_server:start_link({local, RealBroker}, ?MODULE, RealBroker, []).

behaviour_info(callbacks) -> [{init, 0}, {dispatch, 1}, {create_channel, 1}, {destroy_channel, 1}];
behaviour_info(_) -> undefined.

%% @private
init(RealBroker) ->
  RealBrokerState = RealBroker:init(),
  {ok, #state{real_broker = RealBroker, real_broker_state = RealBrokerState, ready_channels = sets:new()}}.

dispatch(Channel, QueuedCall) ->
  Broker = channel:broker(Channel),
  gen_server:call(Broker, {dispatch, Channel, QueuedCall}).

notify_ready(Broker) ->
  gen_server:call(Broker, notify_ready).

%% @private
handle_call({dispatch, Channel, QueuedCall}, _From, State = #state{real_broker = RealBroker, ready_channels = ReadyChannels}) ->
  SessionPid = case QueuedCall#queued_call.session_id of
    undefined ->
      {ok, Pid} = session:new(), Pid;
    SessionId ->
      session:find(SessionId)
  end,

  case session:dial(SessionPid, RealBroker, Channel, QueuedCall) of
    ok ->
      {reply, {ok, SessionPid}, State};
    error ->
      {reply, error, State};
    unavailable ->
      {reply, unavailable, State#state{ready_channels = sets:add_element(Channel#channel.id, ReadyChannels)}}
  end;

handle_call(notify_ready, _From, State = #state{ready_channels = ReadyChannels}) ->
  sets:fold(fun (ChannelId, _) ->
    channel_queue:wakeup(ChannelId)
  end, undefined, ReadyChannels),
  {reply, ok, State#state{ready_channels = sets:new()}};

handle_call(_Request, _From, State) ->
  {reply, {error, unknown_call}, State}.

%% @private
% handle_cast({notify, Queue}, State = #state{ready_channels = ReadyChannels}) ->
%   channel_queue:dispatch(Queue),
%   {noreply, State#state{ready_channels = sets:add_element(Queue, ReadyChannels)}};

handle_cast(Msg, State = #state{real_broker = RealBroker, real_broker_state = RealBrokerState}) ->
  NewState = RealBroker:handle_cast(Msg, RealBrokerState),
  {noreply, State#state{real_broker_state = NewState}}.

%% @private
handle_info(_Info, State) ->
  {noreply, State}.

%% @private
terminate(_Reason, _State) ->
  ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
