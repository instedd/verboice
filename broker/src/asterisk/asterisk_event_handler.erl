-module(asterisk_event_handler).
-behaviour(gen_event).
-export([init/1, handle_event/2, handle_call/2, handle_info/2, terminate/2, code_change/3]).

%% @private
init(_) ->
  {ok, undefined}.

%% @private
handle_event({originateresponse, Packet}, State) ->
  OriginateResponse = ami_client:decode_packet(Packet),
  case proplists:get_value(response, OriginateResponse) of
    <<"Failure">> ->
      SessionId = binary_to_list(proplists:get_value(actionid, OriginateResponse)),
      io:format("Session id: ~p~n", [SessionId]),
      case session:find(SessionId) of
        undefined -> ok;
        SessionPid ->
          io:format("Pid: ~p~n", [SessionPid]),
          Reason = case proplists:get_value(reason, OriginateResponse) of
            <<"3">> -> no_answer;
            <<"5">> -> busy;
            _       -> failed
          end,
          io:format("Reason: ~p~n", [Reason]),
          session:reject(SessionPid, Reason)
      end;
    X -> io:format("response: ~p~n", [X])
  end,
  {ok, State};

handle_event(Event, State) ->
  % io:format("EVENT: ~p~n", [Event]),
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