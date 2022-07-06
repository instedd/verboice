% Active calls GC: Garbage collector for active calls.
% When a call remains active for too long Verboice considers there was an error and cancels it.
% This calls remain "falsely active": they don't have sessions anymore, they are canceled through the call_log info.
% This GC runs every X minutes and cancels every active call for more than N minutes.
% The value X defaults to 10 minutes and is configurable via the ENV variable `minutes_between_active_calls_gc_runs`.
% The value N defaults to 120 minutes and is configurable via the ENV variable `minutes_for_cancelling_active_calls`.
-module(active_calls_gc).
-export([code_change/3, handle_call/3, handle_cast/2, handle_info/2, init/1, start_link/0, terminate/2]).
-compile([{parse_transform, lager_transform}]).
-behaviour(gen_server).

-define(SERVER, ?MODULE).

-include("session.hrl").
-include("db.hrl").
-include("uri.hrl").

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, {}, []).

init({}) ->
  Interval = verboice_config:minutes_between_active_calls_gc_runs(),
  timer:send_interval(timer:minutes(Interval), cancel_active_calls),
  {ok, undefined}.

handle_call(_Request, _From, State) ->
  {reply, {error, unknown_call}, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

notify_failed_to_callback_url(Call) ->
  case Call#call_log.callback_url of
    undefined -> ok;
    <<>> -> ok;
    Url ->
      CallSid = util:to_string(Call#call_log.id),
      SessionVars = case Call#call_log.js_context of
        "" -> [];
        undefined -> [];
        _ -> session:session_vars(Call#call_log.js_context)
      end,
      spawn(fun() ->
        Uri = uri:parse(binary_to_list(Url)),
        QueryString = [{"CallSid", CallSid}, {"CallStatus", "failed"}, {"CallDuration", 0} | SessionVars],
        MergedQueryString = Uri#uri.query_string ++ QueryString,
        NewUri = Uri#uri{query_string = MergedQueryString},
        NewUri:get([{full_result, false}])
      end)
  end.

handle_info(cancel_active_calls, State) ->
  N = verboice_config:minutes_for_cancelling_active_calls(),
  N_Minutes_Ago = util:seconds_ago(N * 60),
  CallsStartedBefore = call_log:find_all([{state, "active"}, {started_at, '<', N_Minutes_Ago}]),

  lists:foreach(fun(Call) ->
      % First the callback url is notified
      notify_failed_to_callback_url(Call),
      % Then the call logs are updated to mark them as failed
      call_log:update(call_log:update(Call#call_log{state ="failed", fail_reason = "active-for-too-long"}))
    end, CallsStartedBefore),
  
  lager:info("GC cancelled ~p calls that stayed active for more than ~p minutes", [length(CallsStartedBefore), N]),
  {noreply, State};

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
