-module(session).
-export([start_link/4]).

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-include("session.hrl").

start_link(SessionId, Pbx, ChannelId, JsRuntime) ->
  gen_server:start_link(?MODULE, {SessionId, Pbx, ChannelId, JsRuntime}, []).

%% @private
init({SessionId, Pbx, ChannelId, JsRuntime}) ->
  [CallFlowId] = db:select_one("SELECT call_flow_id FROM channels WHERE id = ~p", [ChannelId]),
  CallFlow = call_flow:find(CallFlowId),
  Flow = CallFlow:commands(),
  io:format("~p~n", [Flow]),

  {ok, #session{session_id = SessionId, pbx = Pbx, flow = Flow, js_runtime = JsRuntime}}.

%% @private
handle_call(_Request, _From, State) ->
  {reply, {error, unknown_call}, State}.

%% @private
handle_cast(run, State) ->
  io:format("Starting!~n"),
  Pid = spawn_link(fun() -> run(State) end),
  monitor(process, Pid),
  {noreply, State}.

%% @private
handle_info({'DOWN', _Ref, process, _Pid, _}, State) ->
  {stop, normal, State};
handle_info(Info, State) ->
  io:format("~p~n", [Info]),
  {noreply, State}.

%% @private
terminate(_Reason, _State) ->
  ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

run(State = #session{pbx = Pbx, js_runtime = JsRuntime}) ->
  JsContext = mozjs:new_context(JsRuntime),
  RunState = State#session{js_context = JsContext},
  try run(RunState, 1) of
    _ -> ok
  catch
    hangup -> hangup
  after
    Pbx:terminate()
  end.

run(#session{flow = Flow}, Ptr) when Ptr > length(Flow) -> finish;
run(State = #session{flow = Flow}, Ptr) ->
  Command = lists:nth(Ptr, Flow),
  case eval(Command, State) of
    next ->
      run(State, Ptr + 1);
    {goto, N} ->
      run(State, N + 1);
    {exec, NewFlow} ->
      run(State#session{flow = NewFlow}, 1);
    finish -> finish
  end.

eval(stop, _) -> finish;
eval([Command, Args], State) -> Command:run(Args, State);
eval(Command, State) -> Command:run(State).
