-module(scheduler).
-export([start_link/0]).

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-record(state, {last_id, waiting_calls}).

-include("db.hrl").
-foo(something).

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, {}, []).

%% @private
init({}) ->
  gen_server:cast(?SERVER, dispatch),

  {ok, #state{last_id = 0}}.

%% @private
handle_call(_Request, _From, State) ->
  {reply, {error, unknown_call}, State}.

%% @private
handle_cast(dispatch, State) ->
  timer:sleep(1000),
  QueuedCalls = queued_call:find_all([], [{order_by, not_before}]),
  LastId = case QueuedCalls of
    [] -> 0;
    _ -> (lists:max(QueuedCalls))#queued_call.id
  end,

  WaitingCalls = lists:foldl(fun(Call, Queue) ->
    case should_trigger(Call) of
      true ->
        channel_queue:enqueue(Call),
        Queue;
      false ->
        {datetime, NotBefore} = Call#queued_call.not_before,
        ordsets:add_element({NotBefore, Call}, Queue)
    end
  end, ordsets:new(), QueuedCalls),

  {noreply, State#state{last_id = LastId, waiting_calls = WaitingCalls}};

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


should_trigger(#queued_call{not_before = undefined}) -> true;
should_trigger(#queued_call{not_before = {datetime, NotBefore}}) ->
  NotBefore =< calendar:universal_time();
should_trigger(_) -> false.
