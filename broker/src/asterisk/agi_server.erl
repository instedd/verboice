-module(agi_server).
-export([start_link/0]).

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, {}, []).

%% @private
init({}) ->
  {ok, BrokerPort} = application:get_env(broker_port),
  {ok, ListenSock} = gen_tcp:listen(BrokerPort, [{active, false}, {reuseaddr, true}, binary, {packet, line}]),
  spawn_link(fun() -> server(ListenSock) end),
  {ok, ListenSock}.

%% @private
handle_call(_Request, _From, State) ->
  {reply, {error, unknown_call}, State}.

%% @private
handle_cast(_Msg, State) ->
  {noreply, State}.

%% @private
handle_info(_Info, State) ->
  {noreply, State}.

%% @private
terminate(_Reason, ListenSock) ->
  gen_tcp:close(ListenSock),
  ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

server(ListenSock) ->
  {ok, Sock} = gen_tcp:accept(ListenSock),
  agi_session_sup:start_session(Sock),
  server(ListenSock).