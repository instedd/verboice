-module(channel_queue).
-export([start_link/1, whereis_channel/1, enqueue/1, wakeup/1, unmonitor_session/2]).

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(QUEUE(ChannelId), {global, {channel, ChannelId}}).
-define(IDLE_TIMEOUT, 300000).
-include("db.hrl").

-record(state, {self, active = true, channel, current_calls = 0, sessions, max_calls, queued_calls}).

start_link(Channel = #channel{id = Id}) ->
  gen_server:start_link(?QUEUE(Id), ?MODULE, Channel, []).

%% Enqueue a new ready call. The queue will immediately dispatch to broker unless it's busy
enqueue(QueuedCall = #queued_call{channel_id = ChannelId}) ->
  ensure_exists(ChannelId),
  gen_server:cast(?QUEUE(ChannelId), {enqueue, QueuedCall}).

%% Notify the queue that it can now dispatch ready calls to the broker
wakeup(ChannelId) ->
  ensure_exists(ChannelId),
  gen_server:cast(?QUEUE(ChannelId), wakeup).

%% Remove a session from monitored list
unmonitor_session(ChannelId, SessionPid) ->
  ensure_exists(ChannelId),
  gen_server:cast(?QUEUE(ChannelId), {unmonitor, SessionPid}).

whereis_channel(ChannelId) ->
  {global, GlobalName} = ?QUEUE(ChannelId),
  global:whereis_name(GlobalName).

ensure_exists(ChannelId) ->
  case whereis_channel(ChannelId) of
    undefined ->
      ok = channel_mgr:ensure_channel(ChannelId);
    Pid when is_pid(Pid) ->
      ok
  end.

%% @private
init(Channel) ->
  {ok, #state{
    self = ?QUEUE(Channel#channel.id),
    channel = Channel,
    max_calls = Channel:limit(),
    queued_calls = queue:new(),
    sessions = ordsets:new()
  }}.

%% @private
handle_call(_Request, _From, State) ->
  {reply, {error, unknown_call}, State}.

%% @private
handle_cast({enqueue, QueuedCall}, State = #state{queued_calls = Queue}) ->
  Queue2 = queue:in(QueuedCall, Queue),
  exit_state(do_dispatch(State#state{queued_calls = Queue2}));

handle_cast(wakeup, State) ->
  exit_state(do_dispatch(State#state{active = true}));

handle_cast({unmonitor, Pid}, State) ->
  exit_state(remove_session(Pid, State));

handle_cast(_Msg, State) ->
  {noreply, State}.

%% @private
handle_info({'DOWN', _, process, Pid, _}, State) ->
  exit_state(remove_session(Pid, State));

handle_info(timeout, State) ->
  case is_idle(State) of
    true -> {stop, normal, State};
    _    -> {noreply, State}
  end;

handle_info(_Info, State) ->
  {noreply, State}.

%% @private
terminate(_Reason, _State) ->
  ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

is_idle(#state{sessions = [], queued_calls = Q}) -> queue:is_empty(Q);
is_idle(_) -> false.

exit_state(State) ->
  case is_idle(State) of
    true -> {noreply, State, ?IDLE_TIMEOUT};
    _    -> {noreply, State}
  end.

remove_session(SessionPid, State = #state{current_calls = C, sessions = Sessions}) ->
  case ordsets:is_element(SessionPid, Sessions) of
    true ->
      NewSessions = ordsets:del_element(SessionPid, Sessions),
      timer:apply_after(timer:seconds(2), gen_server, cast, [self(), wakeup]),
      State#state{sessions = NewSessions, current_calls = C - 1};
    false ->
      State
  end.

do_dispatch(State = #state{active = false}) -> State;
do_dispatch(State = #state{current_calls = C, max_calls = M}) when C >= M -> State;
do_dispatch(State = #state{current_calls = C, queued_calls = Q, sessions = S}) ->
  case queue:out(Q) of
    {empty, _} -> State;
    {{value, Call}, Q2} ->
      case Call#queued_call.id =:= undefined orelse Call:exists() of
        true ->
          case broker:dispatch(State#state.channel, Call) of
            {ok, SessionPid} ->
              Call:delete(),
              monitor(process, SessionPid),
              NewSessions = ordsets:add_element(SessionPid, S),
              do_dispatch(State#state{current_calls = C + 1, queued_calls = Q2, sessions = NewSessions});
            error ->
              Call:delete(),
              do_dispatch(State#state{queued_calls = Q2});
            unavailable ->
              State#state{active = false}
          end;
        false ->
          error_logger:info_msg("Ignoring queued call ~p which seems has been deleted", [Call#queued_call.id]),
          do_dispatch(State#state{queued_calls = Q2})
      end
  end.

