-module(asterisk_call_manager).

-behaviour(gen_event).
-export([init/1, handle_event/2, handle_call/2, handle_info/2, terminate/2, code_change/3]).

%% @private
init(_) ->
  {ok, undefined}.

%% @private
handle_event({new_session, Pid, Env}, State) ->
  case proplists:get_value(extension, Env) of
    <<"h">> ->
      % Ignore incoming calls of hangup sessions
      agi_session:close(Pid),
      {ok, State};

    _ ->
      Pbx = asterisk_pbx:new(Pid),

      case proplists:get_value(arg_1, Env) of
        Arg1 when is_binary(Arg1), Arg1 =/= <<>> ->
          % Outgoing call
          SessionId = binary_to_list(Arg1),
          case session:find(SessionId) of
            undefined ->
              agi_session:close(Pid),
              {ok, State};
            SessionPid ->
              session:answer(SessionPid, Pbx),
              {ok, State}
          end;

        _ ->
          % Incoming call
          agi_session:ringing(Pid),
          {ok, PeerIp} = agi_session:get_variable(Pid, "CHANNEL(peerip)"),
          SipTo = binary_to_list(proplists:get_value(dnid, Env)),
          ChannelId = case asterisk_channel_srv:find_channel(PeerIp, SipTo) of
            not_found -> list_to_integer(binary_to_list(proplists:get_value(arg_2, Env)));
            Found -> Found
          end,
          CallerId = case proplists:get_value(callerid, Env) of
            <<>> -> undefined;
            <<"unknown">> -> undefined;
            X -> X
          end,

          case session:new() of
            {ok, SessionPid} ->
              session:answer(SessionPid, Pbx, ChannelId, CallerId),
              {ok, State};
            {error, _Reason} ->
              agi_session:close(Pid),
              {ok, State}
          end
      end
  end;

handle_event(_Event, State) ->
  {ok, State}.

%% @private
handle_call(_Request, _State) ->
  {remove_handler, {error, unknown_call}}.

%% @private
handle_info(_, State) ->
  {ok, State}.

%% @private
terminate(_Reason, _State) ->
  ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
