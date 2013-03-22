-module(session).
-export([start_link/2]).

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

start_link(SessionId, Pbx) ->
  gen_server:start_link(?MODULE, {SessionId, Pbx}, []).

%% @private
init({SessionId, Pbx}) ->
  {data, Result} = mysql:fetch(db, "SELECT flow FROM call_flows LIMIT 1"),
  [[Row]] = mysql:get_result_rows(Result),
  Z = zlib:open(),
  zlib:inflateInit(Z),
  [FlowYaml] = zlib:inflate(Z, binary_to_list(Row)),
  {ok, [Flow]} = yaml:load(FlowYaml, [{schema, yaml_schema_ruby}]),
  io:format("~p~n", [Flow]),

  {ok, {SessionId, Pbx, [answer, [play_resource, [{resource_guid, "813482510aca741a243c46d2e4b6f02b"}]], [goto, 2], hangup]}}.

%% @private
handle_call(_Request, _From, State) ->
  {reply, {error, unknown_call}, State}.

%% @private
handle_cast(run, State = {SessionId, Pbx, Flow}) ->
  io:format("Starting!~n"),
  Pid = spawn_link(fun() -> run(SessionId, Pbx, Flow) end),
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

run(SessionId, Pbx, Flow) ->
  try run(SessionId, Pbx, Flow, 1) of
    _ -> ok
  catch
    hangup -> hangup
  after
    Pbx:terminate()
  end.

run(_, _, Flow, Ptr) when Ptr > length(Flow) -> finish;
run(SessionId, Pbx, Flow, Ptr) ->
  Command = lists:nth(Ptr, Flow),
  case eval(Command, Pbx) of
    next ->
      run(SessionId, Pbx, Flow, Ptr + 1);
    {goto, N} ->
      run(SessionId, Pbx, Flow, N);
    finish -> finish
  end.

eval(answer, Pbx) -> Pbx:answer(), next;
eval(hangup, Pbx) -> Pbx:hangup(), next;
eval([goto, N], _Pbx) -> {goto, N};

eval([play_resource, Args], Pbx) ->
  Guid = proplists:get_value(resource_guid, Args),
  Pbx:play(Guid), next.
