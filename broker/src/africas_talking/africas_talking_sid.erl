-module(africas_talking_sid).
-export([start/2, find_session_pid/1]).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-compile([{parse_transform, lager_transform}]).

start(AfricasTalkingSid, SessionPid) ->
  gen_server:start({global, {?MODULE, AfricasTalkingSid}}, ?MODULE, SessionPid, []).

find_session_pid(AfricasTalkingSid) ->
  case global:whereis_name({africas_talking_sid, AfricasTalkingSid}) of
    undefined -> undefined;
    Pid ->
      gen_server:call(Pid, {find_session_pid})
  end.

handle_call({find_session_pid}, _From, SessionPid) ->
  {reply, SessionPid, SessionPid}.

%% @private
init(SessionPid) ->
  monitor(process, SessionPid),
  {ok, SessionPid}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info({'DOWN', _, process, SessionPid, _}, SessionPid) ->
  {stop, normal, SessionPid};

handle_info(Info, State) ->
  lager:info("handle_info: ~p", [Info]),
  {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

terminate(_Reason, _State) ->
  ok.
