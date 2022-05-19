% Active calls GC: Garbage collector for active calls.
% When a call remains active for too long Verboice considers there was an error and cancels it.
% This GC runs every INTERVAL. It cancels every active call for N (default=120) minutes.
% This time (N) is configurable via the ENV variable minutes_for_cancelling_active_calls.
-module(active_calls_gc).
-export([start_link/0]).

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-define(ONE_MINUTE, 60000).
-define(TEN_MINUTES, 600000).

-define(INIT_DELAY, ?ONE_MINUTE).
-define(INTERVAL, ?TEN_MINUTES).

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, {}, []).

init({}) ->
  erlang:send_after(?INIT_DELAY,self(),cancel_active_calls),
  {ok, undefined}.

handle_call(_Request, _From, State) ->
  {reply, {error, unknown_call}, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(cancel_active_calls, State) ->
  N = verboice_config:minutes_for_cancelling_active_calls(),
  Reason = "active_calls_gc N=" ++ integer_to_list(N),
  call_log:cancel_active_calls_for_minutes(N, Reason),
  erlang:send_after(?INTERVAL,self(),cancel_active_calls),
  {noreply, State};

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
