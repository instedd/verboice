-module(session).
-export([start_link/1, new/0, find/1, answer/2, answer/4, dial/4, reject/2, stop/1]).
-export([language/1]).

% FSM Description
% Possible states: ready, dialing, in_progress, suspended, completed
% Initial state: ready
%
% ready(dial) -> dialing
% ready(answer) -> in_progress
% dialing(answer) -> in_progress
% dialing(error | no_answer | busy) -> failed
% in_progress(error | hangup) -> failed
% in_progress(done) -> completed
% in_progress(suspend) -> suspended
% suspended(resume) -> in_progress

-behaviour(gen_fsm).
-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).
-export([ready/2, ready/3, dialing/2, in_progress/2, matches/2]).

-define(SESSION(Id), {session, Id}).

-include("session.hrl").
-include("db.hrl").
-include("uri.hrl").

start_link(SessionId) ->
  gen_fsm:start_link({global, ?SESSION(SessionId)}, ?MODULE, SessionId, []).

new() ->
  SessionId = uuid:to_string(uuid:v4()),
  SessionSpec = {SessionId, {session, start_link, [SessionId]}, temporary, 5000, worker, [session]},
  supervisor:start_child(session_sup, SessionSpec).

find(SessionId) ->
  global:whereis_name(?SESSION(SessionId)).

answer(SessionPid, Pbx, ChannelId, CallerId) ->
  gen_fsm:send_event(SessionPid, {answer, Pbx, ChannelId, CallerId}).

answer(SessionPid, Pbx) ->
  gen_fsm:send_event(SessionPid, {answer, Pbx}).

dial(SessionPid, RealBroker, Channel, QueuedCall) ->
  gen_fsm:sync_send_event(SessionPid, {dial, RealBroker, Channel, QueuedCall}).

reject(SessionPid, Reason) ->
  gen_fsm:send_event(SessionPid, {reject, Reason}).

stop(SessionPid) ->
  gen_fsm:send_all_state_event(SessionPid, stop).

matches(SessionPid, Criteria) ->
  try gen_fsm:sync_send_all_state_event(SessionPid, {matches, Criteria}, 100)
  catch
    exit:_ -> false
  end.

language(#session{js_context = JsContext, default_language = DefaultLanguage}) ->
  case erjs_object:get(var_language, JsContext) of
    undefined -> DefaultLanguage;
    Language -> Language
  end.

%% @private

init(SessionId) ->
  {ok, ready, #session{session_id = SessionId}}.

ready({answer, Pbx, ChannelId, CallerId}, Session) ->
  error_logger:info_msg("Session (~p) answer", [Session#session.session_id]),
  Channel = channel:find(ChannelId),
  CallFlow = call_flow:find(Channel#channel.call_flow_id),
  Project = project:find(CallFlow#call_flow.project_id),
  CallLog = call_log_srv:new(#call_log{
    account_id = Channel#channel.account_id,
    project_id = CallFlow#call_flow.project_id,
    state = "active",
    direction = "incoming",
    channel_id = ChannelId,
    address = CallerId,
    started_at = calendar:universal_time(),
    call_flow_id = CallFlow#call_flow.id
  }),
  Contact = get_contact(CallFlow#call_flow.project_id, CallerId, 1),
  Flow = CallFlow#call_flow.flow,

  NewSession = Session#session{pbx = Pbx, channel = Channel, flow = Flow, call_flow = CallFlow, call_log = CallLog, project = Project, address = CallerId, contact = Contact},
  spawn_run(NewSession),

  {next_state, in_progress, NewSession}.

ready({dial, RealBroker, Channel, QueuedCall}, _From, Session) ->
  error_logger:info_msg("Session (~p) dial", [Session#session.session_id]),
  CallLog = call_log_srv:new(call_log:find(QueuedCall#queued_call.call_log_id)),
  Project = project:find(QueuedCall#queued_call.project_id),
  Contact = get_contact(QueuedCall#queued_call.project_id, QueuedCall#queued_call.address, QueuedCall#queued_call.call_log_id),

  NewSession = Session#session{
    channel = Channel,
    address = QueuedCall#queued_call.address,
    call_log = CallLog,
    queued_call = QueuedCall,
    project = Project,
    contact = Contact
  },

  case RealBroker:dispatch(NewSession) of
    {error, unavailable} ->
      {stop, normal, unavailable, NewSession};
    {error, Reason} ->
      {_, _, NewSession2} = finalize({failed, Reason}, NewSession),
      {stop, normal, error, NewSession2};
    _ ->
      CallLog:info(["Dialing to ", QueuedCall#queued_call.address, " through channel ", Channel#channel.name], []),
      notify_status(ringing, NewSession),
      CallLog:update([{state, "active"}, {fail_reason, undefined}]),
      {reply, ok, dialing, NewSession, timer:minutes(1)}
  end.

dialing({answer, Pbx}, Session) ->
  error_logger:info_msg("Session (~p) answer", [Session#session.session_id]),
  CallFlow = call_flow:find(Session#session.queued_call#queued_call.call_flow_id),
  NewSession = Session#session{pbx = Pbx, flow = CallFlow#call_flow.flow, call_flow = CallFlow},
  notify_status('in-progress', NewSession),
  spawn_run(NewSession),

  {next_state, in_progress, NewSession};

dialing({reject, Reason}, Session = #session{session_id = SessionId, call_log = CallLog}) ->
  error_logger:info_msg("Session (~p) rejected, reason: ~p", [SessionId, Reason]),
  CallLog:error(["Call was rejected. (Reason: ", atom_to_list(Reason),")"], []),
  notify_status('no-answer', Session),
  finalize({failed, Reason}, Session);

dialing(timeout, Session) ->
  notify_status(busy, Session),
  finalize({failed, timeout}, Session).

in_progress({completed, ok}, Session) ->
  notify_status('completed', Session),
  finalize(completed, Session);

in_progress({completed, {error, Reason}}, Session) ->
  notify_status('failed', Session),
  finalize({failed, Reason}, Session).

notify_status(Status, Session = #session{session_id = SessionId, address = Address, callback_params = CallbackParams}) ->
  Project = Session#session.project,
  case Project#project.status_callback_url of
    undefined -> ok;
    <<>> -> ok;
    Url ->
      Uri = uri:parse(binary_to_list(Url)),
      QueryString = [{"CallSid", SessionId}, {"CallStatus", Status}, {"From", Address} | CallbackParams],
      spawn(fun() -> (Uri#uri{query_string = QueryString}):get([{full_result, false}]) end)
  end.

handle_event(stop, _, Session) ->
  {stop, normal, Session}.

handle_sync_event({matches, Criteria}, _From, StateName, Session) ->
  MatchResult = case Criteria of
    {project, ProjectId} ->
      Session#session.project#project.id == ProjectId;
    {channel, ChannelId} ->
      Session#session.channel#channel.id == ChannelId;
    {call_flow, CallFlowId} ->
      Session#session.call_flow#call_flow.id == CallFlowId;
    _ -> false
  end,
  {reply, MatchResult, StateName, Session};

handle_sync_event(_Event, _From, StateName, Session) ->
  {reply, ok, StateName, Session}.

%% @private
handle_info({'DOWN', _Ref, process, _Pid, Reason}, _, Session) ->
  {stop, Reason, Session};

handle_info(Info, _StateName, State) ->
  io:format("~p~n", [Info]),
  {noreply, State}.

%% @private
terminate(Reason, _, #session{session_id = Id}) ->
  error_logger:info_msg("Session (~p) terminated with reason: ~p", [Id, Reason]),
  ok.

%% @private
code_change(_OldVsn, StateName, State, _Extra) ->
  {ok, StateName, State}.

finalize(completed, Session = #session{call_log = CallLog}) ->
  CallLog:update([{state, "completed"}, {finished_at, calendar:universal_time()}]),
  {stop, normal, Session};

finalize({failed, Reason}, Session = #session{call_log = CallLog}) ->
  NewState = case Session#session.queued_call of
    undefined -> "failed";
    QueuedCall ->
      case QueuedCall:reschedule() of
        no_schedule -> failed;
        max_retries ->
          CallLog:error("Max retries exceeded", []),
          "failed";
        #queued_call{not_before = NotBefore} ->
          CallLog:info(["Call rescheduled to start at ", httpd_util:rfc1123_date(calendar:universal_time_to_local_time(NotBefore))], []),
          "queued"
      end
  end,
  CallLog:update([{state, NewState}, {fail_reason, Reason}, {finished_at, calendar:universal_time()}]),
  {stop, normal, Session}.

spawn_run(Session) ->
  SessionPid = self(),
  spawn_monitor(fun() ->
    {Result, #session{js_context = JsContext}} = run(Session),
    Status = erjs_object:get(status, JsContext),
    gen_fsm:send_event(SessionPid, {completed, flow_result(Result, Status)})
  end).

flow_result(ok, "failed") -> {error, "marked as failed"};
flow_result({error, _}, "successful") -> ok;
flow_result(Result, _) -> Result.

default_language(Session = #session{project = Project}) ->
  Language = case Session#session.queued_call of
    undefined -> Project#project.default_language;
    #queued_call{variables = VarsYaml} ->
      {ok, [Vars]} = yaml:load(VarsYaml, [{schema, yaml_schema_ruby}]),
      case proplists:get_value(<<"language">>, Vars) of
        undefined -> Project#project.default_language;
        <<>> -> Project#project.default_language;
        Lang -> Lang
      end
  end,
  binary_to_list(Language).

get_contact(ProjectId, undefined, CallLogId) ->
  Address = "Anonymous" ++ integer_to_list(CallLogId),
  contact:create_anonymous(ProjectId, Address);
get_contact(ProjectId, Address, _) ->
  contact:find_or_create_with_address(ProjectId, Address).

default_variables(#session{contact = Contact, project = #project{id = ProjectId}}) ->
  Context = erjs_object:new([{record_url, fun(_Key) -> "<url>" end}]),
  ProjectVars = project_variable:names_for_project(ProjectId),
  Variables = persisted_variable:find_all({contact_id, Contact#contact.id}),
  default_variables(Context, ProjectVars, Variables).

default_variables(Context, _ProjectVars, []) -> Context;
default_variables(Context, ProjectVars, [#persisted_variable{value = undefined} | Rest]) ->
  default_variables(Context, ProjectVars, Rest);
default_variables(Context, ProjectVars, [Var | Rest]) ->
  VarName = case Var#persisted_variable.implicit_key of
    <<"language">> -> var_language;
    <<"sms_number">> -> var_sms_number;
    undefined -> proplists:get_value(Var#persisted_variable.project_variable_id, ProjectVars)
  end,
  VarValue = binary_to_list(Var#persisted_variable.value),
  default_variables(erjs_object:set(VarName, VarValue, Context), ProjectVars, Rest).

callback_params(#session{queued_call = #queued_call{callback_params = CallbackParamsYaml}}) when is_binary(CallbackParamsYaml) ->
  {ok, [CallbackParams]} = yaml:load(CallbackParamsYaml, [{schema, yaml_schema_ruby}]),
  CallbackParams;
callback_params(_) -> [].

run(Session = #session{pbx = Pbx}) ->
  JsContext = default_variables(Session),
  CallbackParams = callback_params(Session),
  RunSession = Session#session{
    js_context = JsContext,
    default_language = default_language(Session),
    callback_params = CallbackParams
  },
  try run(RunSession, 1) of
    X -> X
  after
    Pbx:terminate()
  end.

run(Session = #session{flow = Flow}, Ptr) when Ptr > length(Flow) -> end_flow(Session);
run(Session = #session{flow = Flow, stack = Stack, call_log = CallLog}, Ptr) ->
  Command = lists:nth(Ptr, Flow),
  try eval(Command, Session) of
    {Action, NewSession} ->
      case Action of
        next ->
          run(NewSession, Ptr + 1);
        {goto, N} ->
          run(NewSession, N + 1);
        {exec, NewFlow} ->
          case has_ended(Flow, Ptr + 1) of
            true -> run(NewSession#session{flow = NewFlow}, 1);
            false -> run(NewSession#session{flow = NewFlow, stack = [{Flow, Ptr + 1} | Stack]}, 1)
          end;
        finish ->
          end_flow(NewSession)
      end
  catch
    hangup ->
      CallLog:info("The user hang up", []),
      {{error, hangup}, Session};
    Class:Error ->
      CallLog:error(["Error ", io_lib:format("~p:~p", [Class, Error])], []),
      error_logger:error_msg("Error during session ~p: ~p:~p~n~p~n",
        [Session#session.session_id, Class, Error, erlang:get_stacktrace()]),
      {{error, error}, Session}
  end.

end_flow(Session = #session{stack = []}) -> {ok, Session};
end_flow(Session = #session{stack = [{Flow, Ptr} | Rest]}) ->
  run(Session#session{flow = Flow, stack = Rest}, Ptr).

has_ended(Flow, Ptr) when Ptr > length(Flow) ->  true;
has_ended(Flow, Ptr) -> lists:nth(Ptr, Flow) =:= stop.

eval(stop, Session) -> {finish, Session};
eval([Command, Args], Session) -> Command:run(Args, Session);
eval(Command, Session) -> Command:run(Session).
