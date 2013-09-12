-module(asterisk_event_handler).
-behaviour(gen_event).
-export([init/1, handle_event/2, handle_call/2, handle_info/2, terminate/2, code_change/3]).

%% @private
init(_) ->
  {ok, undefined}.

%% @private
handle_event(connected, State) ->
  ami_client:login("verboice", "verboice"),
  timer:apply_after(timer:seconds(5), asterisk_broker, notify_ready, []),
  {ok, State};

handle_event({originateresponse, Packet}, State) ->
  OriginateResponse = ami_client:decode_packet(Packet),
  case proplists:get_value(response, OriginateResponse) of
    <<"Failure">> ->
      SessionId = binary_to_list(proplists:get_value(actionid, OriginateResponse)),
      case session:find(SessionId) of
        undefined -> ok;
        SessionPid ->
          Reason = case proplists:get_value(reason, OriginateResponse) of
            <<"3">> -> no_answer;
            <<"5">> -> busy;
            _       -> failed
          end,
          session:reject(SessionPid, Reason)
      end;
    _ -> ok
  end,
  {ok, State};

handle_event({newchannel, Packet}, State) ->
  NewChannel = ami_client:decode_packet(Packet),
  io:format("New channel: ~p~n", [NewChannel]),
  Channel = proplists:get_value(channel, NewChannel),
  asterisk_pbx_log_srv:start_link(Channel),
  {ok, State};

handle_event({varset, Packet}, State) ->
  Event = ami_client:decode_packet(Packet),
  Channel = proplists:get_value(channel, Event),
  asterisk_pbx_log_srv:varset(Channel, Event),
  {ok, State};

handle_event({hangup, Packet}, State) ->
  Event = ami_client:decode_packet(Packet),
  Channel = proplists:get_value(channel, Event),
  asterisk_pbx_log_srv:hangup(Channel, Event),
  {ok, State};

handle_event(_Event, State) ->
  {ok, State}.

%% @private
handle_call(_Request, _State) ->
  {remove_handler, {error, unknown_call}}.

%% @private
handle_info(_Info, State) ->
  {ok, State}.

%% @private
terminate(_Reason, _State) ->
  ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
