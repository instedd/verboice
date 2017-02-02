-module(asterisk_channel_srv).
-compile([{parse_transform, lager_transform}]).
-export([start_link/0, find_channel/2, register_channel/2, regenerate_config/0, set_channel_status/1, get_channel_status/1]).

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("db.hrl").

-define(SERVER, ?MODULE).

% chan_pjsip will stop attempting outbound registrations if it receives a
% 401/403, until the next reload. Since failure counts are incremented on each
% check_status, care must be taken to allow at least two reloads before
% disabling a channel.

-define(RELOAD_INTERVAL, timer:minutes(2)).
-define(STATUS_INTERVAL, timer:seconds(30)).
-define(REGISTRATION_FAILURES_THRESHOLD, 7).

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
% registration_failures: dict(channel_id, {failure_count, last_updated_at})
%   Number of times a registration failures has been seen after last
%   channel update, and the timestamp of the last update. If more than
%   ?REGISTRATION_FAILURES_THRESHOLD, the channel will be automatically disabled.
%
-record(state, {channels, dynamic_channels, registry, channel_status, config_job_state = idle, status_job_state = idle, registration_failures}).

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
  timer:send_interval(?STATUS_INTERVAL, check_status),
  timer:send_after(?RELOAD_INTERVAL, sip_reload),
  {ok, #state{channels = dict:new(), dynamic_channels = dict:new(), registry = dict:new(), registration_failures = dict:new()}}.

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
  case State#state.channel_status of
    undefined -> {reply, [], State};
    Status ->
      Result = lists:foldl(fun(ChannelId, R) ->
        case dict:find(ChannelId, Status) of
          {ok, ChannelStatus} -> [ChannelStatus | R];
          _ -> R
        end
      end, [], ChannelIds),
      {reply, Result, State}
  end;

handle_call(_Request, _From, State) ->
  {reply, {error, unknown_call}, State}.

%% @private
handle_cast(regenerate_config, State = #state{config_job_state = JobState, registration_failures = RegFailures}) ->
  NewState = case JobState of
    idle ->
      spawn_link(fun() ->
        {ok, BaseConfigPath} = application:get_env(asterisk_config_dir),
        ConfigFilePath = filename:join(BaseConfigPath, "pjsip_verboice.conf"),
        {ChannelIndex, RegistryIndex} = asterisk_config:generate(ConfigFilePath),
        ami_client:sip_reload(),
        gen_server:cast(?MODULE, {set_channels, ChannelIndex, RegistryIndex})
      end),
      %% reset the count of registration failures
      NewRegFailures = reset_registration_failures(RegFailures),
      State#state{config_job_state = working, registration_failures = NewRegFailures};
    working ->
      State#state{config_job_state = must_regenerate};
    must_regenerate ->
      State
  end,
  {noreply, NewState};

handle_cast({set_channels, ChannelIndex, RegistryIndex}, State = #state{config_job_state = JobState}) ->
  lager:info("Updated Asterisk Channel registry: ~B IP-number and ~B SIP registrations",
             [dict:size(ChannelIndex), dict:size(RegistryIndex)]),
  lager:debug("Asterisk Channel registry: ~n~p~n~p~n", [ChannelIndex, RegistryIndex]),
  case JobState of
    must_regenerate -> regenerate_config();
    _ -> ok
  end,
  {noreply, State#state{channels = ChannelIndex, registry = RegistryIndex, config_job_state = idle}};

handle_cast({set_channel_status, Status}, State = #state{channel_status = PrevStatus, registration_failures = RegFailures}) ->
  channel:log_broken_channels(PrevStatus, Status),

  % increment the registration failures
  {NewRegFailures, DisableChannels} = update_registration_failures(Status, RegFailures),
  if
    length(DisableChannels) > 0 ->
      % disable channels that have excedeed the number of failed status checks
      channel:disable_by_ids(DisableChannels),
      % and rewrite configuration
      timer:apply_after(0, ?MODULE, regenerate_config, []);
    true ->
      ok
  end,

  {noreply, State#state{channel_status = Status, status_job_state = idle, registration_failures = NewRegFailures}};

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

reset_registration_failures(RegFailures) ->
  Channels = channel:find_all_sip(),
  lists:foldl(reset_registration_failures_folder(RegFailures), dict:new(), Channels).

reset_registration_failures_folder(RegFailures) ->
  fun(#channel{id = ChannelId, updated_at = UpdatedAt}, RegIn) ->
      FailureCount = case dict:find(ChannelId, RegFailures) of
                       {ok, {PrevCount, UpdatedAt}} ->
                         %% maintain failure count only when the
                         %% updated timestamp hasn't changed
                         PrevCount;
                       _ ->
                         0
                     end,
      dict:store(ChannelId, {FailureCount, UpdatedAt}, RegIn)
  end.

update_registration_failures(ChannelStatus, RegFailures) ->
  dict:fold(fun update_registration_failures/3, {RegFailures, []}, ChannelStatus).

update_registration_failures(ChannelId, {_, true, _}, {RegIn, DisableIn}) ->
  {dict:erase(ChannelId, RegIn), DisableIn};
update_registration_failures(ChannelId, {_, false, _}, {RegIn, DisableIn}) ->
  case dict:find(ChannelId, RegIn) of
    {ok, {FailureCount, _}} when FailureCount + 1 >= ?REGISTRATION_FAILURES_THRESHOLD ->
      {dict:erase(ChannelId, RegIn), [ChannelId | DisableIn]};
    {ok, {FailureCount, UpdatedAt}} ->
      {dict:store(ChannelId, {FailureCount + 1, UpdatedAt}, RegIn), DisableIn};
    _ ->
      %% we didn't know about the channel, so ignore it
      {RegIn, DisableIn}
  end.
