% Active calls GC: Garbage collector for active calls.
% When a call remains active for too long Verboice considers there was an error and cancels it.
% This GC runs every X minutes and cancels every active call for more than N minutes.
% The value X defaults to 10 minutes and is configurable via the ENV variable `minutes_between_active_calls_gc_runs`.
% The value N defaults to 120 minutes and is configurable via the ENV variable `minutes_for_cancelling_active_calls`.
-module(active_calls_gc).
-export([code_change/3, handle_call/3, handle_cast/2, handle_info/2, init/1, start_link/0, terminate/2]).
-compile([{parse_transform, lager_transform}]).
-behaviour(gen_server).

-define(SERVER, ?MODULE).

-define(MILLISECS_IN_ONE_MINUTE, 60000).
-define(INIT_DELAY, ?MILLISECS_IN_ONE_MINUTE).

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
  N_Minutes_Ago = util:seconds_ago(N * 60),
  Count = call_log:cancel_active_calls_started_before(N_Minutes_Ago),
  lager:info("GC cancelled ~p calls that stayed active for more than ~p minutes", [Count, N]),
  Interval = ?MILLISECS_IN_ONE_MINUTE * verboice_config:minutes_between_active_calls_gc_runs(),
  erlang:send_after(Interval, self(), cancel_active_calls),
  {noreply, State};

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
