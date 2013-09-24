-module(cache).
-export([start_link/0, get/1, get/2, set/2, delete/1]).

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-include_lib("stdlib/include/ms_transform.hrl").

-define(SERVER, ?MODULE).
-define(TTL, 300).
-record(bucket, {value, expiration}).
-record(ref_link, {key}).
-record(link, {key}).

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, {}, []).

get(Key) ->
  get_result(ets:lookup(cache, Key)).

get(Key, Fun) ->
  case cache:get(Key) of
    undefined ->
      case gen_server:call(?SERVER, {get, Key, Fun}) of
        {error, Type, Ex} -> erlang:Type(Ex);
        Value -> Value
      end;
    Value -> Value
  end.

get_result([]) -> undefined;
get_result([{_, #bucket{value = Value}} | _]) -> Value;
get_result([{_, #link{key = Key}} | _]) -> cache:get(Key);
get_result([_ | Rest]) -> get_result(Rest).

expiration() ->
  {A, B, C} = now(),
  {A, B + ?TTL, C}.

set(Key, Value) ->
  gen_server:cast(?SERVER, {set, Key, Value, expiration()}).

delete(Key) ->
  gen_server:cast(?SERVER, {delete, Key}).

%% @private
init({}) ->
  ets:new(cache, [bag, public, named_table, {read_concurrency, true}]),
  timer:send_interval(timer:minutes(1), delete_expired),
  {ok, undefined}.

%% @private
handle_call({get, Key, Fun}, _From, State) ->
  Response = case cache:get(Key) of
    undefined ->
      try
        case Fun() of
          undefined -> undefined;
          {RealKey, Value} ->
            replace(RealKey, Value, expiration()),
            case RealKey of
              Key -> ok;
              _ ->
                ets:insert(cache, {Key, #link{key = RealKey}}),
                ets:insert(cache, {RealKey, #ref_link{key = Key}})
            end,
            Value
        end
      catch
        Type:Ex -> {error, Type, Ex}
      end;
    Value -> Value
  end,
  {reply, Response, State};

handle_call(_Request, _From, State) ->
  {reply, {error, unknown_call}, State}.

%% @private
handle_cast({set, Key, Value, Expiration}, State) ->
  replace(Key, Value, Expiration),
  {noreply, State};

handle_cast({delete, Key}, State) ->
  internal_delete(Key),
  {noreply, State};

handle_cast(_Msg, State) ->
  {noreply, State}.

%% @private
handle_info(delete_expired, State) ->
  Now = now(),
  ExpiredKeys = ets:select(cache, ets:fun2ms(fun({Key, #bucket{expiration = E}}) when Now > E -> Key end)),
  delete_all(ExpiredKeys),
  {noreply, State};

handle_info(_Info, State) ->
  {noreply, State}.

%% @private
terminate(_Reason, _State) ->
  ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

replace(Key, Value, Expiration) ->
  ets:match_delete(cache, {Key, #bucket{value = '_', expiration = '_'}}),
  ets:insert(cache, {Key, #bucket{value = Value, expiration = Expiration}}).

delete_all([]) -> ok;
delete_all([Key | Others]) ->
  internal_delete(Key),
  delete_all(Others).

internal_delete(Key) ->
  delete_recursive(ets:lookup(cache, Key)).

delete_recursive([]) -> ok;
delete_recursive([{_, #ref_link{key = Key}} | Rest]) ->
  ets:delete(cache, Key),
  delete_recursive(Rest);
delete_recursive([{Key, #bucket{}} | Rest]) ->
  ets:delete(cache, Key),
  delete_recursive(Rest);
delete_recursive([{_, #link{key = Key}} | Rest]) ->
  internal_delete(Key),
  delete_recursive(Rest).
