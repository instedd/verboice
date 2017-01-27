-module(asterisk_channel_srv).
-compile([{parse_transform, lager_transform}]).
-export([start_link/0, find_channel/2, register_channel/2, regenerate_config/0, set_channel_status/1, get_channel_status/1]).

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(RELOAD_INTERVAL, timer:minutes(2)).

% channels: dict({ip, number}, [channel_id])
%   Static host registered peers. This is built by asterisk_config:generate/2.
%
% dynamic_channels: dict({ip, number}, channel_id)
%   Dynamic host connected peers. This is constructed from AMI events, either
%   peerstatus or peerentry (received after a sippeers command is issued)
%
% registry: dict({username, domain}, channel_id)
%   Channels for Asterisk to register, as built by asterisk_config:generate/2.
%
% channel_status: dict(channel_id, {channel_id, registration_ok, error_message})
%   Registration statuses
%
% config_channel_status: dict(channel_id, {channel_id, registration_ok, error_message})
%   Channel configuration errors; if a channel fails to configure correctly,
%   there will be an entry for it here
%
-record(state, {channels, dynamic_channels, registry, channel_status, config_channel_status, config_job_state = idle, status_job_state = idle}).

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, {}, []).

find_channel(PeerIp, SipTo) ->
  gen_server:call(?MODULE, {find_channel, PeerIp, SipTo}).

register_channel(ChannelId, PeerIp) ->
  gen_server:call(?MODULE, {register_channel, ChannelId, PeerIp}).

regenerate_config() ->
  gen_server:cast(?MODULE, regenerate_config).

set_channel_status(Status) ->
  gen_server:cast(?MODULE, {set_channel_status, Status}).

get_channel_status(ChannelIds) ->
  gen_server:call(?MODULE, {get_channel_status, ChannelIds}).

%% @private
init({}) ->
  agi_events:add_sup_handler(asterisk_call_manager, []),
  regenerate_config(),
  timer:send_interval(timer:seconds(30), check_status),
  timer:send_after(?RELOAD_INTERVAL, sip_reload),
  {ok, #state{channels = dict:new(), dynamic_channels = dict:new(), registry = dict:new()}}.

%% @private
handle_call({find_channel, PeerIp, Number}, _From, State) ->
  case dict:find({PeerIp, Number}, State#state.channels) of
    {ok, [ChannelId]} ->
      {reply, ChannelId, State};
    {ok, ChannelIds} ->
      % Return the channel id which has registered successfully, or the first
      % of the list.  This ensures that if a user is attempting to steal calls
      % from another user (by registering a channel on the same IP address/es
      % and the same number), he must also successfully register with the SIP
      % peer (ie. know the credentials).
      ChannelId = case find_registered_channel(ChannelIds, State#state.channel_status) of
        undefined -> lists:nth(1, ChannelIds);
        Other -> Other
      end,
      {reply, ChannelId, State};
    error ->
      case dict:find({PeerIp, Number}, State#state.dynamic_channels) of
        {ok, ChannelId} -> {reply, ChannelId, State};
        error -> {reply, not_found, State}
      end
  end;

handle_call({register_channel, ChannelId, PeerIp}, _From, State) ->
  NewState = case channel:find(ChannelId) of
    undefined ->
      State;
    Channel ->
      NewDynChannels = dict:store({PeerIp, channel:number(Channel)}, ChannelId, State#state.dynamic_channels),
      State#state{dynamic_channels = NewDynChannels}
  end,
  {reply, ok, NewState};

handle_call({get_channel_status, ChannelIds}, _From, State) ->
  Result = lists:foldl(fun(ChannelId, R) ->
    case find_channel_status(ChannelId, State) of
      undefined -> R;
      ChannelStatus -> [ChannelStatus | R]
    end
  end, [], ChannelIds),
  {reply, Result, State};

handle_call(_Request, _From, State) ->
  {reply, {error, unknown_call}, State}.


%% @private
handle_cast(regenerate_config, State = #state{config_job_state = JobState}) ->
  NewState = case JobState of
    idle ->
      spawn_link(fun() ->
        {ok, BaseConfigPath} = application:get_env(asterisk_config_dir),
        ConfigFilePath = filename:join(BaseConfigPath, "pjsip_verboice.conf"),
        {ChannelIndex, RegistryIndex, ConfigErrors} = asterisk_config:generate(ConfigFilePath),
        ami_client:sip_reload(),
        gen_server:cast(?MODULE, {set_channels, ChannelIndex, RegistryIndex, ConfigErrors})
      end),
      State#state{config_job_state = working, config_channel_status = undefined};
    working ->
      State#state{config_job_state = must_regenerate};
    must_regenerate ->
      State
  end,
  {noreply, NewState};

handle_cast({set_channels, ChannelIndex, RegistryIndex, ConfigErrors}, State = #state{config_job_state = JobState}) ->
  lager:info("Updated Asterisk Channel registry: ~B IP-number and ~B SIP registrations, ~B errors",
             [dict:size(ChannelIndex), dict:size(RegistryIndex), length(ConfigErrors)]),
  lager:debug("Asterisk Registrations: ~p", [RegistryIndex]),
  lager:debug("Asterisk Channels: ~p", [ChannelIndex]),
  lager:debug("Asterisk Configuration Errors: ~p", [ConfigErrors]),
  case JobState of
    must_regenerate -> regenerate_config();
    _ -> ok
  end,
  ConfigChannelStatus = dict:from_list(lists:map(fun(Value = {ChannelId, _, _}) -> {ChannelId, Value} end, ConfigErrors)),
  {noreply, State#state{channels = ChannelIndex, registry = RegistryIndex, config_channel_status = ConfigChannelStatus, config_job_state = idle}};

handle_cast({set_channel_status, Status}, State = #state{channel_status = PrevStatus}) ->
  channel:log_broken_channels(PrevStatus, Status),
  {noreply, State#state{channel_status = Status, status_job_state = idle}};

handle_cast(_Msg, State) ->
  {noreply, State}.

%% @private
handle_info(check_status, State = #state{status_job_state = idle}) ->
  case asterisk_status_handler:start(State#state.registry) of
    ok -> {noreply, State#state{status_job_state = working}};
    _ -> {noreply, State#state{channel_status = undefined}}
  end;

handle_info(sip_reload, State) ->
  ami_client:sip_reload(),
  timer:send_after(?RELOAD_INTERVAL, sip_reload),
  {noreply, State};

handle_info({gen_event_EXIT, asterisk_call_manager, Reason}, State) ->
  {stop, Reason, State};

handle_info(_Info, State) ->
  {noreply, State}.

%% @private
terminate(_Reason, _State) ->
  ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

% returns the first channel id in the list that successfully registered according to the ChannelStatus dict()
find_registered_channel(_, undefined) ->
  undefined;
find_registered_channel(ChannelIds, ChannelStatus) ->
  lists:foldl(fun
      (ChannelId, undefined) ->
        case dict:find(ChannelId, ChannelStatus) of
          {ok, {ChannelId, true, _}} -> ChannelId;
          _ -> undefined
        end;
      (_, Result) ->
        Result
    end, undefined, ChannelIds).

find_channel_status(ChannelId, #state{channel_status = ChannelStatus, config_channel_status = ConfigChannelStatus}) ->
  case find_channel_status(ChannelId, ChannelStatus) of
    undefined ->
      find_channel_status(ChannelId, ConfigChannelStatus);
    Status ->
      Status
  end;

find_channel_status(_, undefined) ->
  undefined;
find_channel_status(ChannelId, StatusDict) ->
  case dict:find(ChannelId, StatusDict) of
    {ok, Status} ->
      Status;
    _ ->
      undefined
  end.
