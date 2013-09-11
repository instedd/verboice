-module(asterisk_pbx_log_srv).
-include("db.hrl").
-export([start_link/1, varset/2, hangup/2]).

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER(Channel), {global, {?MODULE, Channel}}).

start_link(Channel) ->
  gen_server:start_link(?SERVER(Channel), ?MODULE, Channel, []).

varset(Channel, Event) ->
  gen_server:cast(?SERVER(Channel), {varset, Event}).

hangup(Channel, Event) ->
  gen_server:cast(?SERVER(Channel), {hangup, Event}).

create_log(Guid, Details) ->
  PbxLog = #pbx_log{guid = Guid, details = Details},
  PbxLog:create().

%% @private
init(Channel) ->
  Guid = uuid:to_string(uuid:v4()),
  create_log(Guid, ["New SIP channel: ", Channel]),
  {ok, Guid, timer:minutes(10)}.

%% @private
handle_call(_Request, _From, Guid) ->
  {reply, {error, unknown_call}, Guid}.

%% @private
handle_cast({varset, Event}, Guid) ->
  Variable = proplists:get_value(variable, Event),
  Value = proplists:get_value(value, Event),
  case Variable of
    <<"verboice_session_id">> ->
      call_log_srv:associate_pbx_log(binary_to_list(Value), Guid);
    <<"\~HASH\~SIP_CAUSE\~SIP", _/binary>> ->
      create_log(Guid, Value);
    _ ->
      create_log(Guid, [Variable, " = ", Value])
  end,
  {noreply, Guid};

handle_cast({hangup, Event}, Guid) ->
  Reason = proplists:get_value('cause-txt', Event),
  create_log(Guid, ["Channel hangup. Reason: ", Reason]),
  {stop, normal, Guid};

handle_cast(_Msg, Guid) ->
  {noreply, Guid}.

%% @private
handle_info(timeout, Guid) ->
  {stop, normal, Guid};

handle_info(_Info, Guid) ->
  {noreply, Guid}.

%% @private
terminate(_Reason, _Guid) ->
  ok.

%% @private
code_change(_OldVsn, Guid, _Extra) ->
  {ok, Guid}.
