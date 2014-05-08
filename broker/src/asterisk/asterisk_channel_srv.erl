-module(asterisk_channel_srv).
-export([start_link/0, find_channel/2, regenerate_config/0, set_channel_status/1, get_channel_status/1]).

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(RELOAD_INTERVAL, timer:minutes(2)).

-record(state, {channels, registry, channel_status, config_job_state = idle, status_job_state = idle}).

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, {}, []).

find_channel(PeerIp, SipTo) ->
  gen_server:call(?MODULE, {find_channel, PeerIp, SipTo}).

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
  {ok, #state{channels = dict:new(), registry = dict:new()}}.

%% @private
handle_call({find_channel, PeerIp, Number}, _From, State) ->
  case dict:find({PeerIp, Number}, State#state.channels) of
    {ok, ChannelId} -> {reply, ChannelId, State};
    error -> {reply, not_found, State}
  end;

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
handle_cast(regenerate_config, State = #state{config_job_state = JobState}) ->
  NewState = case JobState of
    idle ->
      spawn_link(fun() ->
        {ok, BaseConfigPath} = application:get_env(asterisk_config_dir),
        RegFilePath = filename:join(BaseConfigPath, "sip_verboice_registrations.conf"),
        ChannelsFilePath = filename:join(BaseConfigPath, "sip_verboice_channels.conf"),
        {ChannelIndex, RegistryIndex} = asterisk_config:generate(RegFilePath, ChannelsFilePath),
        ami_client:sip_reload(),
        gen_server:cast(?MODULE, {set_channels, ChannelIndex, RegistryIndex})
      end),
      State#state{config_job_state = working};
    working ->
      State#state{config_job_state = must_regenerate};
    must_regenerate ->
      State
  end,
  {noreply, NewState};

handle_cast({set_channels, ChannelIndex, RegistryIndex}, State = #state{config_job_state = JobState}) ->
  io:format("Updated registry: ~n~p~n~p~n", [ChannelIndex, RegistryIndex]),
  case JobState of
    must_regenerate -> regenerate_config();
    _ -> ok
  end,
  {noreply, State#state{channels = ChannelIndex, registry = RegistryIndex, config_job_state = idle}};

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
