-module(scheduler).
-export([start_link/0, load/0, enqueue/1]).

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-record(state, {last_id, waiting_calls}).

-include("db.hrl").
-foo(something).

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, {}, []).

load() ->
  gen_server:cast(?SERVER, load).

enqueue(Call) ->
  gen_server:cast(?SERVER, {enqueue, Call}).

%% @private
init({}) ->
  % Initial load after 1 second
  timer:apply_after(timer:seconds(1), ?MODULE, load, []),

  % Load queued calls every 1 minute, just in case the broker misses a notification
  % about new queued calls.
  timer:apply_interval(timer:minutes(1), ?MODULE, load, []),

  % Check every 10 seconds for due calls
  timer:send_interval(timer:seconds(10), dispatch),

  {ok, #state{last_id = 0, waiting_calls = gb_sets:empty()}}.

%% @private
handle_call(_Request, _From, State) ->
  {reply, {error, unknown_call}, State}.

%% @private
handle_cast(load, State) ->
  {noreply, load_queued_calls(State)};

handle_cast({enqueue, #queued_call{call_log_id = Id}}, State = #state{last_id = LastId})
  when is_integer(Id), Id > LastId ->
  {noreply, State};
handle_cast({enqueue, Call}, State = #state{waiting_calls = WaitingCalls}) ->
  {noreply, State#state{waiting_calls = enqueue(Call, WaitingCalls)}};

handle_cast(_Msg, State) ->
  {noreply, State}.

%% @private
handle_info(dispatch, State = #state{waiting_calls = WaitingCalls}) ->
  WaitingCalls2 = dispatch(WaitingCalls),
  {noreply, State#state{waiting_calls = WaitingCalls2}};

handle_info(_Info, State) ->
  {noreply, State}.

%% @private
terminate(_Reason, _State) ->
  ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

load_queued_calls(State = #state{last_id = LastId}) ->
  case queued_call:find_all([{call_log_id, '>', LastId}], [{order_by, call_log_id}, {limit, 5000}]) of
    [] -> State;
    LoadedCalls ->
      NewState = lists:foldl(fun process_call/2, State, LoadedCalls),
      load_queued_calls(NewState)
  end.

process_call(Call, State = #state{last_id = LastId, waiting_calls = WaitingCalls}) ->
  NewWaitingCalls = case should_trigger(Call) of
    true ->
      channel_queue:enqueue(Call),
      WaitingCalls;
    false ->
      enqueue(Call, WaitingCalls)
  end,
  State#state{last_id = max(Call#queued_call.call_log_id, LastId), waiting_calls = NewWaitingCalls}.

enqueue(Call, WaitingCalls) ->
  {datetime, NotBefore} = Call#queued_call.not_before,
  gb_sets:add_element({NotBefore, Call}, WaitingCalls).

dispatch(Queue) ->
  case gb_sets:is_empty(Queue) of
    true -> Queue;
    _ ->
      {{_, Call}, Queue2} = gb_sets:take_smallest(Queue),
      case should_trigger(Call) of
        true ->
          channel_queue:enqueue(Call),
          dispatch(Queue2);
        false ->
          Queue
      end
  end.

should_trigger(#queued_call{not_before = undefined}) -> true;
should_trigger(#queued_call{not_before = {datetime, NotBefore}}) ->
  NotBefore =< calendar:universal_time();
should_trigger(_) -> false.
