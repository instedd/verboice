-module(asterisk_channel_srv).
-export([start_link/0]).

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {channels}).
-include_lib("kernel/include/inet.hrl").
-include("db.hrl").

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, {}, []).

%% @private
init({}) ->
  spawn_link(fun() -> timer:sleep(2000), generate_config() end),
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
handle_cast({set_channels, Registry}, State) ->
  io:format("Updated registry: ~p~n", [Registry]),
  {noreply, State#state{channels = Registry}};
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
  ChannelRegistry = generate_config(Channels, dict:new()),
  gen_server:cast(?MODULE, {set_channels, ChannelRegistry}).

generate_config([], ChannelRegistry) -> ChannelRegistry;
generate_config([Channel | Rest], ChannelRegistry) ->
  NewRegistry = case channel:domain(Channel) of
    <<>> -> ChannelRegistry;
    Domain ->
      Expanded = expand_domain(Domain),
      Number = channel:number(Channel),
      lists:foldl(fun ({_Host, IPs, _Port}, R1) ->
        lists:foldl(fun (IP, R2) ->
          dict:store({binary_to_list(IP), Number}, Channel#channel.id, R2)
        end, R1, IPs)
      end, ChannelRegistry, Expanded)
  end,
  generate_config(Rest, NewRegistry).

expand_domain(<<>>) -> [];
expand_domain(Domain) ->
  Query = binary_to_list(iolist_to_binary(["_sip._udp.", Domain])),
  case inet_res:getbyname(Query, srv) of
    {ok, #hostent{h_addr_list = AddrList}} ->
      lists:map(fun ({_, _, Port, Host}) ->
        case inet_res:gethostbyname(Host) of
          {ok, #hostent{h_addr_list = IpList}} ->
            IPs = lists:map(fun ({A,B,C,D}) -> iolist_to_binary(io_lib:format("~B.~B.~B.~B", [A,B,C,D])) end, IpList),
            {Host, IPs, Port};
          _ -> {Host, [], Port}
        end
      end, AddrList);
    _ ->
      case inet_res:gethostbyname(Domain) of
        {ok, #hostent{h_addr_list = IpList}} ->
          IPs = lists:map(fun ({A,B,C,D}) -> iolist_to_binary(io_lib:format("~B.~B.~B.~B", [A,B,C,D])) end, IpList),
          [{Domain, IPs, undefined}];
        _ -> [{Domain, [Domain], undefined}]
      end
  end.

