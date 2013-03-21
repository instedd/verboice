-module(session_srv).
% -export([
%   start/0,
%   next/1
% ]).
-export([start/1, stop/1]).
-export([start_link/0]).

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, {}, []).

%% @private
init({}) ->
  {ok, undefined}.

%% @private
handle_call({start, Pbx}, _From, State) ->
  SessionId = make_ref(),
  {ok, Pid} = supervisor:start_child(session_sup, {SessionId, {session, start_link, [SessionId, Pbx]}, temporary, 5000, worker, [session]}),
  gen_server:cast(Pid, run),
  {reply, ok, State};

handle_call({stop, Pbx}, _From, State) ->
  supervisor:terminate_child(session_sup, Pbx),
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

start(Pbx) ->
  gen_server:call(?SERVER, {start, Pbx}).

stop(Pbx) ->
  gen_server:call(?SERVER, {stop, Pbx}).
