-module(asterisk_channel_srv).
-export([start_link/0, find_channel/2, regenerate_config/0]).

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {channels, config_job_state = idle}).

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, {}, []).

find_channel(PeerIp, SipTo) ->
  gen_server:call(?MODULE, {find_channel, PeerIp, SipTo}).

regenerate_config() ->
  gen_server:cast(?MODULE, regenerate_config).

%% @private
init({}) ->
  agi_events:add_handler(asterisk_call_manager, []),
  regenerate_config(),
  {ok, #state{channels = dict:new()}}.

%% @private
handle_call({find_channel, PeerIp, Number}, _From, State) ->
  case dict:find({PeerIp, Number}, State#state.channels) of
    {ok, ChannelId} -> {reply, ChannelId, State};
    error -> {reply, not_found, State}
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
        ChannelRegistry = asterisk_config:generate(RegFilePath, ChannelsFilePath),
        ami_client:sip_reload(),
        gen_server:cast(?MODULE, {set_channels, ChannelRegistry})
      end),
      State#state{config_job_state = working};
    working ->
      State#state{config_job_state = must_regenerate};
    must_regenerate ->
      State
  end,
  {noreply, NewState};

handle_cast({set_channels, Registry}, State = #state{config_job_state = JobState}) ->
  io:format("Updated registry: ~n~p~n", [Registry]),
  case JobState of
    must_regenerate -> regenerate_config();
    _ -> ok
  end,
  {noreply, State#state{channels = Registry, config_job_state = idle}};

handle_cast(_Msg, State) ->
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
