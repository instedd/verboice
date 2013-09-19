-module(call_log_entry_srv).
-export([start_link/0, log/4, trace/5]).

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-include("db.hrl").

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, {}, []).

log(CallLogId, Level, Message, Details) ->
  gen_server:cast(?SERVER, {log, CallLogId, Level, Message, Details}).

trace(CallLogId, CallFlowId, StepId, StepName, Result) ->
  gen_server:cast(?SERVER, {trace, CallLogId, CallFlowId, StepId, StepName, Result}).

%% @private
init({}) ->
  {ok, DbName} = application:get_env(db_name),
  {ok, DbUser} = application:get_env(db_user),
  {ok, DbPass} = application:get_env(db_pass),
  mysql:connect(log, "localhost", undefined, DbUser, DbPass, DbName, true),
  db:set_db(log),
  {ok, undefined}.

%% @private
handle_call(_Request, _From, State) ->
  {reply, {error, unknown_call}, State}.

%% @private
handle_cast({log, CallLogId, Level, Message, Details}, State) ->
  call_log_entry:create(Level, CallLogId, Message, Details),
  {noreply, State};

handle_cast({trace, CallLogId, CallFlowId, StepId, StepName, Result}, State) ->
  TraceRecord = #trace_record{
    call_flow_id = CallFlowId,
    step_id = StepId,
    step_name = StepName,
    call_id = CallLogId,
    result = Result
  },
  TraceRecord:save(),
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
