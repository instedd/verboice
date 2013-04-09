-module(asterisk_call_manager).
-behaviour(gen_event).

-include_lib("eastrisk/include/agi.hrl").

-export([
  code_change/3,
  handle_call/2,
  handle_event/2,
  handle_info/2,
  init/1,
  terminate/2
]).

code_change(_, _, _) -> {error}.
handle_call(_, _) -> {error}.
init(_) -> {ok, dict:new()}.
terminate(_, _) -> ok.

handle_event({new_channel, Pid, Env}, State) ->
  io:format("New call ~p~n", [Env]),
  {ok, {1, PeerIp}} = agi:get_variable(Pid, "CHANNEL(peerip)"),
  SipTo = Env#agi_env.dnid,
  ChannelId = gen_server:call(asterisk_channel_srv, {find_channel, PeerIp, SipTo}),
  Pbx = asterisk_pbx:new(Pid),
  try session_srv:start(Pbx, ChannelId) of
    {ok, SessionId} ->
      monitor(process, Pid),
      {ok, dict:store(Pid, SessionId, State)};
    {error, _Reason} ->
      agi_channel:close(Pid),
      {ok, State}
  catch
    _:_ ->
      agi_channel:close(Pid),
      {ok, State}
  end;

handle_event(Event, State) ->
  io:format("Unhandled event: ~p~n", Event),
  {ok, State}.


handle_info({'DOWN', _Ref, process, Pid, _}, State) ->
  case dict:find(Pid, State) of
    {ok, SessionId} ->
      session_srv:stop(SessionId),
      {ok, dict:erase(Pid, State)};
    _ -> {ok, State}
  end;

handle_info(_, State) ->
  {ok, State}.



