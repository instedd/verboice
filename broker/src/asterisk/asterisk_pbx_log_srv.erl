-module(asterisk_pbx_log_srv).
-include("db.hrl").
-export([start_link/1, varset/2, hangup/2, associate_call_log/2]).

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER(Channel), {global, {?MODULE, Channel}}).
-record(state, {guid, session_id}).

start_link(Channel) ->
  gen_server:start_link(?SERVER(Channel), ?MODULE, Channel, []).

varset(Channel, Event) ->
  gen_server:cast(?SERVER(Channel), {varset, Event}).

hangup(Channel, Event) ->
  gen_server:cast(?SERVER(Channel), {hangup, Event}).

create_log(Guid, Details) ->
  PbxLog = #pbx_log{guid = Guid, details = Details},
  PbxLog:create().

associate_call_log(Channel, SessionId) ->
  gen_server:cast(?SERVER(Channel), {associate_call_log, SessionId}).

%% @private
init(Channel) ->
  Guid = uuid:to_string(uuid:v4()),
  create_log(Guid, ["New SIP channel: ", Channel]),
  {ok, #state{guid=Guid}, timer:minutes(10)}.

%% @private
handle_call(_Request, _From, State) ->
  {reply, {error, unknown_call}, State}.

%% @private
handle_cast({varset, Event}, State=#state{guid=Guid}) ->
  Variable = proplists:get_value(variable, Event),
  Value = proplists:get_value(value, Event),
  NewState = case Variable of
    <<"verboice_session_id">> ->
      SessionId = binary_to_list(Value),
      call_log_srv:associate_pbx_log(SessionId, Guid),
      State#state{session_id=SessionId};
    <<"\~HASH\~SIP_CAUSE\~SIP", _/binary>> ->
      create_log(Guid, Value),
      State;
    _ ->
      create_log(Guid, [Variable, " = ", Value]),
      State
  end,
  {noreply, NewState};

handle_cast({hangup, Event}, State=#state{guid=Guid,session_id=SessionId}) ->
  Reason = proplists:get_value('cause-txt', Event),
  Code = proplists:get_value(cause, Event),
  create_log(Guid, ["Channel hangup. Reason: ", Reason]),
  call_log_srv:hangup(SessionId, {Code, Reason}),
  {stop, normal, State};

handle_cast({associate_call_log, Value}, State=#state{guid=Guid}) ->
  call_log_srv:associate_pbx_log(Value, Guid),
  {noreply, State#state{session_id=Value}};

handle_cast(_Msg, State) ->
  {noreply, State}.

%% @private
handle_info(timeout, State) ->
  {stop, normal, State};

handle_info(_Info, State) ->
  {noreply, State}.

%% @private
terminate(_Reason, _Guid) ->
  ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
