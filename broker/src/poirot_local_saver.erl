-module(poirot_local_saver).
-behaviour(gen_event).
-export([init/1, handle_event/2, handle_call/2, handle_info/2, terminate/2, code_change/3]).

-export([start/0]).
-include("db.hrl").
-include_lib("poirot/include/poirot.hrl").

start() ->
  poirot_event:add_sup_handler(?MODULE, []).

init(_) ->
  {ok, undefined}.

handle_event(Event = #event{type = end_activity, body = Body}, State) ->
  case proplists:get_value(<<"@fields">>, Body) of
    {struct, Fields} ->
      case proplists:get_value(call_log_id, Fields) of
        undefined -> ok;
        CallId ->
          case proplists:get_value(step_type, Fields) of
            undefined -> ok;
            _ ->
              End = proplists:get_value(<<"@end">>, Body),
              EventJson = poirot_event:dump(Event#event{body = [{<<"@start">>, End} | Body]}),
              CallLogEntry = #call_log_entry{call_id = CallId, severity = info, details = [{activity, EventJson}]},
              CallLogEntry:create()
          end
      end;
    _ -> ok
  end,
  {ok, State};

handle_event(_, State) ->
  {ok, State}.

handle_call(_, State) ->
  {ok, {error, unknown_call}, State}.

handle_info(_, State) ->
  {ok, State}.

terminate(_Reason, State) ->
  {ok, State}.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
