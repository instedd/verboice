-module(session).
-export([start_link/1, new/0, find/1, answer/2, answer/4, dial/4, reject/2, stop/1, resume/1, default_variables/1, create_default_erjs_context/2, id/1]).
-export([language/1]).
-compile([{parse_transform, lager_transform}]).

% FSM Description
% Possible states: ready, dialing, in_progress, completed
% Initial state: ready
%
% ready(dial) -> dialing
% ready(answer) -> in_progress
% dialing(answer) -> in_progress
% dialing(error | no_answer | busy) -> failed
% in_progress(error | hangup) -> failed
% in_progress(done) -> completed
% in_progress(suspend) -> ready

-behaviour(gen_fsm).
-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).
-export([ready/2, ready/3, dialing/2, in_progress/2, in_progress/3, matches/2]).

-define(SESSION(Id), {session, Id}).

% delay for executing the job to push call results to Fusion Tables
-define(PUSH_DELAY_SECONDS, 60).

-include("session.hrl").
-include("db.hrl").
-include("uri.hrl").

-record(state, {session_id, session, resume_ptr, pbx_pid, flow_pid, hibernated}).

start_link(HibernatedSession = #hibernated_session{session_id = SessionId}) ->
  StringSessionId = util:to_string(SessionId),
  gen_fsm:start_link({global, ?SESSION(StringSessionId)}, ?MODULE, HibernatedSession, []);
start_link(SessionId) ->
  StringSessionId = util:to_string(SessionId),
  gen_fsm:start_link({global, ?SESSION(StringSessionId)}, ?MODULE, StringSessionId, []).

new() ->
  SessionId = uuid:to_string(uuid:v4()),
  SessionSpec = {SessionId, {session, start_link, [SessionId]}, temporary, 5000, worker, [session]},
  supervisor:start_child(session_sup, SessionSpec).

new(HibernatedSession) ->
  HibernatedSessionId = HibernatedSession#hibernated_session.session_id,
  SessionSpec = {util:to_string(HibernatedSessionId), {session, start_link, [HibernatedSession]}, temporary, 5000, worker, [session]},
  supervisor:start_child(session_sup, SessionSpec).

-spec find(binary() | string()) -> undefined | pid().
find(SessionId) ->
  SessionPid = global:whereis_name(?SESSION(util:to_string(SessionId))),
  case SessionPid of
    undefined ->
      HibernatedSession = hibernated_session:find({session_id, SessionId}),
      case HibernatedSession of
        undefined -> undefined;
        _ ->
          {ok, NewSessionPid} = new(HibernatedSession),
          NewSessionPid
      end;
    _ -> SessionPid
  end.

-spec answer(pid(), pbx:pbx(), integer(), binary()) -> any().
answer(SessionPid, Pbx, ChannelId, CallerId) ->
  gen_fsm:send_event(SessionPid, {answer, Pbx, ChannelId, CallerId}).

-spec answer(pid(), pbx:pbx()) -> any().
answer(SessionPid, Pbx) ->
  gen_fsm:send_event(SessionPid, {answer, Pbx}).

dial(SessionPid, RealBroker, Channel, QueuedCall) ->
  gen_fsm:sync_send_event(SessionPid, {dial, RealBroker, Channel, QueuedCall}).

reject(SessionPid, Reason) ->
  gen_fsm:send_event(SessionPid, {reject, Reason}).

stop(SessionPid) ->
  gen_fsm:send_all_state_event(SessionPid, stop).

resume(SessionPid) ->
  gen_fsm:send_event(SessionPid, resume).

matches(SessionPid, Criteria) ->
  try gen_fsm:sync_send_all_state_event(SessionPid, {matches, Criteria}, 100)
  catch
    exit:_ -> false
  end.

language(#session{js_context = JsContext, default_language = DefaultLanguage}) ->
  case erjs_context:get(var_language, JsContext) of
    undefined -> DefaultLanguage;
    Language -> Language
  end.

id(SessionPid) ->
  gen_fsm:sync_send_all_state_event(SessionPid, id).

%% @private
init(HibernatedSession = #hibernated_session{ data = #hibernated_session_data{resume_ptr = ResumePtr, poirot_activity = Activity }}) ->
  poirot:set_current(Activity),
  Session = HibernatedSession:wake_up(),
  {ok, ready, #state{session_id = Session#session.session_id, session = Session, resume_ptr = ResumePtr}};

init(SessionId) ->
  poirot:push(poirot:new_activity("Session ~p", [SessionId])),
  {ok, ready, #state{session_id = SessionId}}.

ready({answer, Pbx, ChannelId, CallerId}, State = #state{session_id = SessionId}) ->
  lager:info("Session (~p) answer", [SessionId]),
  monitor(process, Pbx:pid()),

  Channel = channel:find(ChannelId),
  StartedAt = calendar:universal_time(),

  case Channel#channel.call_flow_id of
    undefined ->
      CallLog = call_log_srv:new(SessionId, #call_log{
        account_id = Channel#channel.account_id,
        state = "active",
        direction = "incoming",
        channel_id = ChannelId,
        address = CallerId,
        started_at = StartedAt
      }),
      Session = #session{
        call_log = CallLog
      },
      Pbx:reject(),
      Pbx:terminate(),
      finalize({failed, no_call_flow_id}, State#state{session = Session});

    CallFlowId ->
      NewSession = case State#state.session of
        undefined ->
          CallFlow = call_flow:find(CallFlowId),
          Project = project:find(CallFlow#call_flow.project_id),
          Contact = get_contact(CallFlow#call_flow.project_id, CallerId, 1),
          CallLog = call_log_srv:new(SessionId, #call_log{
            account_id = Channel#channel.account_id,
            project_id = CallFlow#call_flow.project_id,
            state = "active",
            direction = "incoming",
            channel_id = ChannelId,
            address = CallerId,
            started_at = StartedAt,
            call_flow_id = CallFlowId,
            contact_id = Contact#contact.id
          }),
          Flow = call_flow:flow(CallFlow),
          {StatusUrl, StatusUser, StatusPass, StatusIncludeVars} = project:status_callback(Project),

          #session{
            session_id = SessionId,
            pbx = Pbx,
            channel = Channel,
            flow = Flow,
            call_flow = CallFlow,
            call_log = CallLog,
            project = Project,
            address = CallerId,
            contact = Contact,
            status_callback_url = StatusUrl,
            status_callback_include_vars = StatusIncludeVars,
            status_callback_user = StatusUser,
            status_callback_password = StatusPass,
            started_at = StartedAt
          };
        Session -> Session#session{pbx = Pbx}
      end,

      poirot:add_meta([
        {address, CallerId},
        {project_id, NewSession#session.project#project.id},
        {call_log_id, (NewSession#session.call_log):id()},
        {channel_id, ChannelId},
        {channel_name, NewSession#session.channel#channel.name}
      ]),

      notify_status('in-progress', NewSession),
      FlowPid = spawn_run(NewSession, State#state.resume_ptr),

      {next_state, in_progress, State#state{pbx_pid = Pbx:pid(), flow_pid = FlowPid, session = NewSession}}
  end.

ready({dial, _, _, QueuedCall = #queued_call{address = undefined}}, _From, State) ->
  lager:error("Refusing to make a call to an undefined address (queued call id ~p)", [QueuedCall#queued_call.id]),
  CallLog = call_log:find(QueuedCall#queued_call.call_log_id),
  CallLog:update([{state, "failed"}, {fail_reason, "undefined address"}, {finished_at, calendar:universal_time()}]),
  {stop, normal, error, State};

ready({dial, RealBroker, Channel, QueuedCall}, _From, State = #state{session_id = SessionId, resume_ptr = ResumePtr}) ->
  lager:info("Session (~p) dial", [SessionId]),

  NewSession = #session{call_log = CallLog, flow = Flow} = prepare_session(QueuedCall, Channel, State),

  case queued_call:should_skip(QueuedCall) of
    true ->
      {_, _, NewSession3} = finalize(overdue, State#state{session = NewSession}),
      {stop, normal, ok, NewSession3};
    false ->
      %% Don't set the started_at if we are resuming an existing session
      case ResumePtr of
        undefined ->
          CallLog:update({started_at, calendar:universal_time()});
        _ ->
          ok
      end,

      case Flow of
        undefined ->
          {_, _, NewSession1} = finalize({failed, {error, "Invalid Flow"}}, State#state{session = NewSession}),
          {stop, normal, error, State#state{session = NewSession1}};
        _ ->
          case RealBroker:dispatch(NewSession) of
            {error, unavailable} ->
              {stop, normal, unavailable, State#state{session = NewSession}};
            {error, Reason} ->
              {_, _, NewSession2} = finalize({failed, Reason}, State#state{session = NewSession}),
              {stop, normal, error, State#state{session = NewSession2}};
            _ ->
              lager:info("Dialing to ~s through channel ~s", [QueuedCall#queued_call.address, Channel#channel.name]),
              notify_status(ringing, NewSession),
              CallLog:update([{state, "active"}, {fail_reason, undefined}]),
              {reply, ok, dialing, State#state{session = NewSession}, timer:minutes(2)}
          end
      end
  end.

dialing({answer, Pbx}, State = #state{session_id = SessionId, session = Session, resume_ptr = Ptr}) ->
  lager:info("Session (~p) answer", [SessionId]),
  monitor(process, Pbx:pid()),
  NewSession = Session#session{pbx = Pbx, started_at = calendar:universal_time() },
  notify_status('in-progress', NewSession),
  FlowPid = spawn_run(NewSession, Ptr),

  {next_state, in_progress, State#state{pbx_pid = Pbx:pid(), flow_pid = FlowPid, session = NewSession}};

dialing({reject, Reason}, State = #state{session = Session = #session{session_id = SessionId, call_log = CallLog}}) ->
  lager:info("Session (~p) rejected, reason: ~p", [SessionId, Reason]),
  CallLog:error(["Call was rejected. (Reason: ", atom_to_list(Reason),")"], []),
  Status = case Reason of
    busy -> busy;
    no_answer -> 'no-answer';
    _ -> failed
  end,
  notify_status(Status, Session, Reason),
  finalize({failed, Reason}, State);

dialing(timeout, State = #state{session = Session}) ->
  notify_status('no-answer', Session),
  finalize({failed, no_answer}, State).

in_progress({completed, NewSession, ok}, State) ->
  notify_status(completed, NewSession),
  finalize(completed, State#state{session = NewSession});

in_progress({completed, NewSession, {failed, Reason}}, State) ->
  notify_status(failed, NewSession, Reason),
  finalize({failed, Reason}, State#state{session = NewSession}).

in_progress({suspend, NewSession, Ptr}, _From, State = #state{session = Session = #session{session_id = SessionId}}) ->
  lager:info("Session (~p) suspended", [SessionId]),
  channel_queue:unmonitor_session(Session#session.channel#channel.id, self()),
  {reply, ok, ready, State#state{pbx_pid = undefined, flow_pid = undefined, resume_ptr = Ptr, session = NewSession}};

in_progress({hibernate, NewSession, Ptr}, _From, State = #state{session = _Session = #session{session_id = SessionId}}) ->
  lager:info("Session (~p) hibernated", [SessionId]),
  Data = #hibernated_session_data{
    flow = NewSession#session.flow,
    stack = NewSession#session.stack,
    js_context = NewSession#session.js_context,
    channel_id = NewSession#session.channel#channel.id,
    call_flow = NewSession#session.call_flow,
    call_log_id = (NewSession#session.call_log):id(),
    project_id = NewSession#session.project#project.id,
    address = NewSession#session.address,
    contact_id = NewSession#session.contact#contact.id,
    default_language = NewSession#session.default_language,
    status_callback_url = NewSession#session.status_callback_url,
    status_callback_include_vars = NewSession#session.status_callback_include_vars,
    status_callback_user = NewSession#session.status_callback_user,
    status_callback_password = NewSession#session.status_callback_password,
    resume_ptr = Ptr,
    poirot_activity = poirot:current()
  },
  HibernatedSession = #hibernated_session{session_id = SessionId, data = Data},
  HibernatedSession:create(),
  {stop, normal, ok, State#state{hibernated = true}}.


prepare_session(QueuedCall, Channel, #state{session_id = SessionId, session = undefined}) ->
  Session = QueuedCall:start_session(),
  NewCallLog = call_log_srv:new(SessionId, call_log:find(QueuedCall#queued_call.call_log_id)),
  NewCallLog:update([{callback_url, Session#session.status_callback_url}, {js_context, Session#session.js_context}]),
  Contact = get_contact(QueuedCall#queued_call.project_id, QueuedCall#queued_call.address, QueuedCall#queued_call.call_log_id),

  poirot:add_meta([
                   {address, QueuedCall#queued_call.address},
                   {project_id, QueuedCall#queued_call.project_id},
                   {call_log_id, QueuedCall#queued_call.call_log_id},
                   {channel_id, Channel#channel.id},
                   {channel_name, Channel#channel.name}
                  ]),

  Session#session{
    session_id = SessionId,
    channel = Channel,
    call_log = NewCallLog,
    contact = Contact
   };

prepare_session(QueuedCall, _, #state{session = Session}) ->
  Session#session{queued_call = QueuedCall, address = QueuedCall#queued_call.address}.

notify_status(Status, Session) ->
  notify_status(Status, Session, undefined).

notify_status(Status, Session, Reason) ->
  notify_status_to_callback_url(Status, Session, Reason),
  notify_status_to_hub(Status, Session).

notify_status_to_callback_url(Status, Session = #session{call_log = CallLog, address = Address, callback_params = CallbackParams, started_at = StartedAt, js_context = JS}, Reason) ->
  % A step on the FSM has happened, JS context could have varied, update it.
  CallLog:update([{js_context, JS}]),
  case Session#session.status_callback_url of
    undefined -> ok;
    <<>> -> ok;
    Url ->
      CallSid = util:to_string(CallLog:id()),
      SessionVars = case Session#session.status_callback_include_vars of
        true -> session_vars(JS);
        1 -> session_vars(JS);
        _ -> []
      end,
      spawn(fun() ->
        Uri = uri:parse(binary_to_list(Url)),
        Duration = case StartedAt of
          undefined -> 0;
          _ ->
            StartedAtSeconds = calendar:datetime_to_gregorian_seconds(StartedAt),
            Now = calendar:universal_time(),
            calendar:datetime_to_gregorian_seconds(Now) - StartedAtSeconds
        end,
        CallReasonParams = case Reason of
          undefined -> [];
          busy -> [];
          no_answer -> [];
          hangup -> [{"CallStatusReason", "Busy"}];
          {internal_error, ErrDetails} -> [{"CallStatusReason", ErrDetails}];
          {error, ErrDetails} -> [{"CallStatusReason", ErrDetails}];
          {error, ErrDetails, ErrCode} -> [{"CallStatusReason", ErrDetails}, {"CallStatusCode", ErrCode}];
          _ ->
            case CallLog:hangup_status() of
              ok -> [];
              {fail, HangupCode, HangupReason} ->
                [{"CallStatusReason", HangupReason}, {"CallStatusCode", HangupCode}]
            end
        end,
        QueryString = [{"CallSid", CallSid}, {"CallStatus", Status}, {"From", Address}, {"CallDuration", erlang:integer_to_list(Duration)} | (CallReasonParams ++ CallbackParams ++ SessionVars)],
        AuthOptions = case Session#session.status_callback_user of
          undefined -> [];
          [] -> [];
          <<>> -> [];
          User -> [{basic_auth, {User, Session#session.status_callback_password}}]
        end,
        MergedQueryString = Uri#uri.query_string ++ QueryString,
        NewUri = Uri#uri{query_string = MergedQueryString},
        NewUri:get([{full_result, false} | AuthOptions])
      end)
  end.

notify_status_to_hub(Status, Session = #session{call_log = CallLog, js_context = JS, project = Project}) ->
  case Status of
    completed ->
      HubEnabled = verboice_config:hub_enabled(),
      case HubEnabled of
        false ->
          ok;
        true ->
          Task = [
            {payload, [
              {call_id, CallLog:id()},
              {project_id, Project:id()},
              {call_flow_id, case Session#session.call_flow of
                undefined -> undefined;
                #call_flow{id = Id} -> Id
              end},
              {address, Session#session.address},
              {vars, {map, session_vars(JS)}}
            ]}
          ],
          delayed_job:enqueue("Jobs::HubJob", Task)
      end;
    _ ->
       ok
  end.

session_vars(undefined) ->
  [];
session_vars(JS) ->
  lists:foldl(fun({Name, Value}, Vars) ->
    case Name of
      <<"var_", VarName/binary>> -> [{binary_to_list(VarName), Value} | Vars];
      _ -> Vars
    end
  end, [], erjs_context:to_list(JS)).

handle_event(stop, _, State) ->
  {stop, normal, State}.

handle_sync_event(id, _From, StateName, State = #state{session_id = SessionId}) ->
  {reply, SessionId, StateName, State};

handle_sync_event({matches, Criteria}, _From, StateName, State = #state{session = Session}) ->
  MatchResult = case Criteria of
    {project, ProjectId} ->
      Session#session.project#project.id == ProjectId;
    {channel, ChannelId} ->
      Session#session.channel#channel.id == ChannelId;
    {call_flow, CallFlowId} ->
      Session#session.call_flow#call_flow.id == CallFlowId;
    _ -> false
  end,
  {reply, MatchResult, StateName, State};

handle_sync_event(_Event, _From, StateName, State) ->
  {reply, ok, StateName, State}.

%% @private
handle_info({'DOWN', _Ref, process, Pid, Reason}, _, State = #state{session = Session, pbx_pid = Pid}) ->
  notify_status(failed, Session, "PBX unexpected error"),
  lager:error("PBX closed unexpectedly with reason: ~s", [Reason]),
  finalize({failed, {internal_error, Reason}}, State);

handle_info({'DOWN', _Ref, process, Pid, Reason}, _, State = #state{session = Session, flow_pid = Pid}) ->
  Pbx = Session#session.pbx,
  Pbx:terminate(),
  notify_status(failed, Session, "Verboice flow unexpected error"),
  lager:error("Flow process died unexpectedly with reason: ~s", [Reason]),
  finalize({failed, {internal_error, Reason}}, State);

handle_info(_Info, StateName, State) ->
  {next_state, StateName, State}.

%% @private
terminate(_Reason, _, #state{hibernated = true}) ->
  ok;
terminate(Reason, _, #state{session_id = SessionId, session = Session}) ->
  push_results(Session),
  case Reason of
    normal -> lager:info("Session (~p) terminated normally", [SessionId]);
    _ -> lager:warning("Session (~p) terminated with reason: ~p", [SessionId, Reason])
  end,
  poirot:pop().

%% @private
code_change(_OldVsn, StateName, State, _Extra) ->
  {ok, StateName, State}.

push_results(#session{call_flow = #call_flow{id = CallFlowId, store_in_fusion_tables = 1}, call_log = CallLog}) ->
  Task = [{call_flow_id, CallFlowId}, {call_log_id, CallLog:id()}],
  delayed_job:enqueue({struct, "CallFlow::FusionTablesPush::Pusher"}, Task, ?PUSH_DELAY_SECONDS);
push_results(_) -> ok.

finalize(completed, State = #state{session = Session = #session{call_log = CallLog}}) ->
  verboice_telemetry:track_call_finished(Session),
  CallLog:update([{state, "completed"}, {finished_at, calendar:universal_time()}]),
  {stop, normal, State};

finalize({failed, Reason}, State = #state{session = Session = #session{call_log = CallLog}}) ->
  verboice_telemetry:track_call_finished(Session),
  NewState = case Session#session.queued_call of
    undefined -> "failed";
    QueuedCall ->
      case QueuedCall:reschedule() of
        no_schedule -> failed;
        overdue ->
          CallLog:error("'Not After' date exceeded", []),
          notify_status("expired", Session),
          "expired";
        max_retries ->
          CallLog:error("Max retries exceeded", []),
          "failed";
        #queued_call{not_before = {datetime, NotBefore}} ->
          CallLog:info(["Call rescheduled to start at ", httpd_util:rfc1123_date(calendar:universal_time_to_local_time(NotBefore))], []),
          "queued"
      end
  end,
  lager:info("Call failed with reason ~p and new state ~p", [Reason, NewState]),
  FailInfo = case Reason of
    hangup ->                    [{fail_reason, "hangup"}];
    busy ->                      [{fail_reason, "busy"}];
    no_answer ->                 [{fail_reason, "no-answer"}];
    no_call_flow_id ->           [{fail_reason, "no-call-flow"}, {fail_details, "No Incoming Flow Defined"}];
    {error, ErrDetails} ->       [{fail_reason, "error"}, {fail_details, ErrDetails}];
    {error, ErrDetails, Code} -> [{fail_reason, "error"}, {fail_details, ErrDetails}, {fail_code, Code}];
    {internal_error, Details} -> [{fail_reason, "internal error"}, {fail_details, io_lib:format("~p", [Details])}];
    _ ->                         [{fail_reason, "unknown error"}]
  end,
  ok = CallLog:update([{state, NewState}, {finished_at, calendar:universal_time()}] ++ FailInfo),
  StopReason = case Reason of
    {error, ErrDetails2} -> ErrDetails2;
    {error, ErrDetails2, _} -> ErrDetails2;
    {internal_error, FatalError} -> FatalError;
    _ -> normal
  end,
  {stop, StopReason, State};

finalize(overdue, State = #state{session = Session = #session{call_log = CallLog}}) ->
  CallLog:update([{state, "expired"}, {finished_at, calendar:universal_time()}]),
  notify_status("expired", Session),
  {stop, normal, State}.

spawn_run(Session = #session{project = Project}, undefined) ->
  JsContext = default_variables(Session),
  RunSession = Session#session{js_context = JsContext, default_language = project:default_language(Project)},
  spawn_run(RunSession, 1);

spawn_run(Session = #session{pbx = Pbx}, Ptr) ->
  SessionPid = self(),
  SessionActivity = poirot:current(),
  {Pid, _Ref} = spawn_monitor(fun() ->
    poirot:new_inside(SessionActivity, "Session worker process", async, fun() ->
      lager:info("Start"),
      try run(Session, Ptr) of
        {suspend, NewSession, NewPtr} ->
          close_user_step_activity(NewSession),
          gen_fsm:sync_send_event(SessionPid, {suspend, NewSession#session{in_user_step_activity = false}, NewPtr});
        {hibernate, NewSession, NewPtr} ->
          close_user_step_activity(NewSession),
          gen_fsm:sync_send_event(SessionPid, {hibernate, NewSession#session{in_user_step_activity = false}, NewPtr});
        {Result, NewSession = #session{js_context = JsContext}} ->
          close_user_step_activity(NewSession),
          Status = erjs_context:get(status, JsContext),
          gen_fsm:send_event(SessionPid, {completed, NewSession, flow_result(Result, Status)})
      after
        catch Pbx:terminate()
      end
    end)
  end),
  Pid.

close_user_step_activity(#session{in_user_step_activity = true}) -> poirot:pop();
close_user_step_activity(_) -> ok.

flow_result(ok, "failed") -> {failed, "marked as failed"};
flow_result({failed, _}, "successful") -> ok;
flow_result(Result, _) -> Result.

get_contact(ProjectId, undefined, CallLogId) ->
  Address = "Anonymous" ++ integer_to_list(CallLogId),
  contact:create_anonymous(ProjectId, Address);
get_contact(ProjectId, Address, _) ->
  contact:find_or_create_with_address(ProjectId, Address).

default_variables(#session{address = Address, contact = Contact, queued_call = QueuedCall, project = #project{id = ProjectId}, call_log = CallLog}) ->
  CallLogId = util:to_string(CallLog:id()),
  Context = create_default_erjs_context(CallLogId, Address),
  ProjectVars = project_variable:names_for_project(ProjectId),
  Variables = persisted_variable:find_all({contact_id, Contact#contact.id}),
  DefaultContext = default_variables(Context, ProjectVars, Variables),
  initialize_context(DefaultContext, QueuedCall).

create_default_erjs_context(CallLogId, PhoneNumber) ->
  erjs_context:new([
    {record_url, fun(Key) ->
      BaseUrl = verboice_config:base_url(),
      BaseUrl ++ "/calls/" ++ CallLogId ++ "/results/" ++ util:to_string(Key)
    end},
    {'_get_var', fun(Name, Context) ->
      Value = erjs_context:get(Name, Context),
      case Value of
        undefined ->
          VarName = binary_to_atom(iolist_to_binary(["var_", atom_to_list(Name)]), utf8),
          erjs_context:get(VarName, Context);
        _ -> Value
      end
    end},
    {'split_digits', fun
      (undefined) ->
        [];
      (Value) ->
        StringValue = if
          is_integer(Value) -> integer_to_list(Value);
          true -> Value
        end,
        Result = re:replace(StringValue,"\\d"," &",[{return,list}, global]),
        Result
      end},
    {phone_number, util:to_string(PhoneNumber)},
    {<<"hub_url">>, verboice_config:hub_url()}
  ]).

initialize_context(Context, #queued_call{variables = Vars}) ->
  lists:foldl(fun({Name, Value}, C) ->
    case Value of
      undefined -> C;
      [] -> C;
      <<>> -> C;
      _ ->
        VarName = binary_to_atom(iolist_to_binary(["var_", Name]), utf8),
        erjs_context:set(VarName, Value, C)
    end
  end, Context, Vars);
initialize_context(Context, _) -> Context.

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
  default_variables(erjs_context:set(VarName, VarValue, Context), ProjectVars, Rest).

run(Session = #session{flow = Flow}, Ptr) when Ptr > length(Flow) -> end_flow(Session);
run(Session = #session{flow = Flow, stack = Stack}, Ptr) ->
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
          end_flow(NewSession);
        suspend ->
          {suspend, NewSession, Ptr + 1};
        hibernate ->
          {hibernate, NewSession, Ptr + 1}
      end
  catch
    hangup ->
      lager:warning("The user hung up"),
      poirot:add_meta([{error, <<"The user hung up">>}]),
      {{failed, hangup}, Session};
    {hangup, NewSession} ->
      lager:warning("The user hung up"),
      poirot:add_meta([{error, <<"The user hung up">>}]),
      {{failed, hangup}, NewSession};
    Reason when is_list(Reason) ->
      poirot:add_meta([{error, iolist_to_binary(io_lib:format("~s", [Reason]))}]),
      lager:error("~s", [Reason]),
      {{failed, Reason}, Session};
    Reason ->
      poirot:add_meta([{error, iolist_to_binary(io_lib:format("~p", [Reason]))}]),
      lager:error("~p", [Reason]),
      {{failed, Reason}, Session};
    Class:Error ->
      poirot:add_meta([{error, iolist_to_binary(io_lib:format("Fatal Internal Error: ~p", [Error]))}]),
      lager:error("Error during session ~p: ~p:~p~n~p~n",
        [Session#session.session_id, Class, Error, erlang:get_stacktrace()]),
      {{failed, {internal_error, Error}}, Session}
  end.

end_flow(Session = #session{stack = []}) -> {ok, Session};
end_flow(Session = #session{stack = [{Flow, Ptr} | Rest]}) ->
  run(Session#session{flow = Flow, stack = Rest}, Ptr).

has_ended(Flow, Ptr) when Ptr > length(Flow) ->  true;
has_ended(Flow, Ptr) -> lists:nth(Ptr, Flow) =:= stop.

eval(stop, Session) -> {finish, Session};
eval([Command, Args], Session) -> Command:run(Args, Session);
eval(Command, Session) -> Command:run(Session).
