-module(session_srv).
% -export([
%   start/0,
%   next/1
% ]).
-export([start/2, stop/1]).
-export([start_link/0]).

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, {}, []).

%% @private
init({}) ->
  {ok, mozjs:new_runtime()}.

%% @private
handle_call({start, Pbx, ChannelId}, _From, State) ->
  SessionId = make_ref(),
  JsRuntime = State,
  SessionSpec = {SessionId, {session, start_link, [SessionId, Pbx, ChannelId, JsRuntime]}, temporary, 5000, worker, [session]},
  case supervisor:start_child(session_sup, SessionSpec) of
    {ok, Pid} ->
      gen_server:cast(Pid, run),
      {reply, {ok, SessionId}, State};
    Error = {error, _} ->
      {reply, Error, State}
  end;

handle_call({stop, SessionId}, _From, State) ->
  session:stop(SessionId),
  {reply, ok, State}.


%% @private
handle_cast(_Msg, State) ->
  {noreply, State}.

%% @private
handle_info(_Info, State) ->
  {noreply, State}.

%% @private
terminate(_Reason, _State) ->
  ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

start(Pbx, ChannelId) ->
  gen_server:call(?SERVER, {start, Pbx, ChannelId}).

stop(SessionId) ->
  gen_server:call(?SERVER, {stop, SessionId}).
