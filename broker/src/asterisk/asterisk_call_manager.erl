-module(asterisk_call_manager).
-behaviour(gen_event).

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

handle_event({new_session, Pid, Env}, State) ->
  io:format("New call ~p~n", [Env]),
  Pbx = asterisk_pbx:new(Pid),

  case proplists:get_value(arg_1, Env) of
    Arg1 when is_binary(Arg1), Arg1 =/= <<>> ->
      % Outgoing call
      SessionId = binary_to_list(Arg1),
      session:answer(SessionId, Pbx), % TODO: what if session does not exist?
      {ok, State};

    _ ->
      % Incoming call
      {ok, PeerIp} = agi_session:get_variable(Pid, "CHANNEL(peerip)"),
      SipTo = binary_to_list(proplists:get_value(dnid, Env)),
      ChannelId = asterisk_channel_srv:find_channel(PeerIp, SipTo),

      case session:new() of
        {ok, SessionId} ->
          io:format("Answering..."),
          monitor(process, Pid), % TODO: let the session monitor the pbx pid
          session:answer(SessionId, Pbx, ChannelId),
          {ok, dict:store(Pid, SessionId, State)};
        {error, _Reason} ->
          agi_session:close(Pid),
          {ok, State}
      end
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

