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

  {ok, #state{last_id = 0, waiting_calls = ordsets:new()}}.

%% @private
handle_call(_Request, _From, State) ->
  {reply, {error, unknown_call}, State}.

%% @private
handle_cast(load, State = #state{last_id = LastId}) ->
  LoadedCalls = queued_call:find_all([{call_log_id, '>', LastId}], [{order_by, not_before}]),
  NewState = lists:foldl(fun process_call/2, State, LoadedCalls),

  {noreply, NewState};

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
  ordsets:add_element({NotBefore, Call}, WaitingCalls).

dispatch([]) -> [];
dispatch(Queue = [{_, Call} | Rest]) ->
  case should_trigger(Call) of
    true ->
      channel_queue:enqueue(Call),
      dispatch(Rest);
    false ->
      Queue
  end.

should_trigger(#queued_call{not_before = undefined}) -> true;
should_trigger(#queued_call{not_before = {datetime, NotBefore}}) ->
  NotBefore =< calendar:universal_time();
should_trigger(_) -> false.
