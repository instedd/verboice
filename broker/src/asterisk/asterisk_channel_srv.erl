-module(asterisk_channel_srv).
-export([start_link/0, find_channel/2, regenerate_config/0]).

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {channels, config_job_state = idle}).
-include_lib("kernel/include/inet.hrl").
-include("db.hrl").

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
      spawn_link(fun() -> generate_config() end),
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


generate_config() ->
  Channels = channel:find_all_sip(),
  ChannelRegistry = generate_config(Channels, dict:new(), dict:new()),
  gen_server:cast(?MODULE, {set_channels, ChannelRegistry}).

generate_config([], _ResolvCache, ChannelRegistry) -> ChannelRegistry;
generate_config([Channel | Rest], ResolvCache, ChannelRegistry) ->
  NewRegistry = case channel:domain(Channel) of
    <<>> ->
      NewCache = ResolvCache,
      ChannelRegistry;
    Domain ->
      {Expanded, NewCache} = expand_domain(Domain, ResolvCache),
      Number = channel:number(Channel),
      lists:foldl(fun ({_Host, IPs, _Port}, R1) ->
        lists:foldl(fun (IP, R2) ->
          dict:store({binary_to_list(IP), Number}, Channel#channel.id, R2)
        end, R1, IPs)
      end, ChannelRegistry, Expanded)
  end,
  generate_config(Rest, NewCache, NewRegistry).

expand_domain(<<>>, ResolvCache) -> {[], ResolvCache};
expand_domain(Domain, ResolvCache) ->
  case dict:find(Domain, ResolvCache) of
    {ok, Expanded} -> {Expanded, ResolvCache};
    _ ->
      case is_ip(Domain) of
        true -> {[{Domain, [Domain], undefined}], ResolvCache};
        _ ->
          Expanded = expand_domain(Domain),
          NewCache = dict:store(Domain, Expanded, ResolvCache),
          {Expanded, NewCache}
      end
  end.

expand_domain(Domain) ->
  Query = binary_to_list(iolist_to_binary(["_sip._udp.", Domain])),
  case inet_res:getbyname(Query, srv) of
    {ok, #hostent{h_addr_list = AddrList}} ->
      [
        case inet_res:gethostbyname(Host) of
          {ok, #hostent{h_addr_list = IpList}} ->
            IPs = map_ips(IpList),
            {Host, IPs, Port};
          _ -> {Host, [], Port}
        end
      || {_, _, Port, Host} <- AddrList];
    _ ->
      case inet_res:gethostbyname(Domain) of
        {ok, #hostent{h_addr_list = IpList}} ->
          IPs = map_ips(IpList),
          [{Domain, IPs, undefined}];
        _ -> [{Domain, [Domain], undefined}]
      end
  end.

is_ip(Address) ->
  case re:run(Address, "^(\\d+\\.){3}\\d+$") of
    {match, _} -> true;
    _ -> false
  end.

map_ips(IpList) ->
  [iolist_to_binary(io_lib:format("~B.~B.~B.~B", [A,B,C,D])) || {A,B,C,D} <- IpList].
