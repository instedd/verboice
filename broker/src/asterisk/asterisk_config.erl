-module(asterisk_config).
-export([generate/2, expand_domain/1]).

-include_lib("kernel/include/inet.hrl").
-include("db.hrl").

generate(RegFilePath, ChannelsFilePath) ->
  {ok, RegFile} = file:open(RegFilePath, [write]),
  try file:open(ChannelsFilePath, [write]) of
    {ok, ChannelsFile} ->
      try generate_config(RegFile, ChannelsFile) of
        R -> R
      after
        file:close(ChannelsFile)
      end
  after
    file:close(RegFile)
  end.

% returns a tuple with the channels index and registrations index for the generated files
%
% ChannelIndex = dict({ip, number}, [channel_id])
% RegistryIndex = dict({username, domain}, channel_id)
%
% ChannelIndex's values are lists since after DNS resolution there might be
% domain names mapped to identical IP addresses. A list of more than one
% element might indicate someone attempting to steal SIP calls from another
% user.
generate_config(RegFile, ChannelsFile) ->
  Channels = channel:find_all_sip(),
  generate_config(Channels, RegFile, ChannelsFile, dict:new(), dict:new(), dict:new()).

generate_config([], _, _, _, ChannelIndex, RegistryIndex) -> {ChannelIndex, RegistryIndex};
generate_config([Channel | Rest], RegFile, ChannelsFile, ResolvCache, ChannelIndex, RegistryIndex) ->
  Section = ["verboice_", integer_to_list(Channel#channel.id)],
  Username = channel:username(Channel),
  Password = channel:password(Channel),
  Domain = channel:domain(Channel),
  Number = channel:number(Channel),

  case Domain of
    [] ->
      file:write(ChannelsFile, ["[", Section, "]\n"]),
      file:write(ChannelsFile, "type=friend\n"),
      file:write(ChannelsFile, "canreinvite=no\n"),
      file:write(ChannelsFile, "nat=yes\n"),
      file:write(ChannelsFile, "qualify=yes\n"),
      file:write(ChannelsFile, ["secret=", Password, "\n"]),
      file:write(ChannelsFile, "insecure=no\n"),
      file:write(ChannelsFile, "context=verboice\n"),
      file:write(ChannelsFile, "host=dynamic\n"),
      file:write(ChannelsFile, "\n"),
      generate_config(Rest, RegFile, ChannelsFile, ResolvCache, ChannelIndex, RegistryIndex);

    _ ->
      file:write(ChannelsFile, ["[", Section, "](!)\n"]),
      file:write(ChannelsFile, "type=peer\n"),
      file:write(ChannelsFile, "canreinvite=no\n"),
      file:write(ChannelsFile, "nat=yes\n"),
      file:write(ChannelsFile, "qualify=yes\n"),

      if length(Username) > 0 andalso length(Password) > 0 ->
        file:write(ChannelsFile, ["fromuser=", Username, "\n"]),
        file:write(ChannelsFile, ["defaultuser=", Username, "\n"]),
        file:write(ChannelsFile, ["secret=", Password, "\n"]);
        true -> ok
      end,

      file:write(ChannelsFile, "insecure=invite,port\n"),
      file:write(ChannelsFile, "context=verboice\n"),
      file:write(ChannelsFile, "\n"),

      case channel:is_outbound(Channel) of
        true ->
          file:write(ChannelsFile, ["[", Section, "-outbound](", Section, ")\n"]),
          file:write(ChannelsFile, ["host=", Domain, "\n"]),
          file:write(ChannelsFile, ["domain=", Domain, "\n"]),
          file:write(ChannelsFile, ["fromdomain=", Domain, "\n"]),
          file:write(ChannelsFile, ["type=peer\n"]),
          file:write(ChannelsFile, "\n");
        _ -> ok
      end,

      {Expanded, NewCache} = expand_domain(Domain, ResolvCache),
      {NewChannelIndex, _} = lists:foldl(fun ({Host, IPs, Port}, {R1, I}) ->
        file:write(ChannelsFile, ["[", Section, "-inbound-", integer_to_list(I), "](", Section, ")\n"]),
        file:write(ChannelsFile, ["host=", Host, "\n"]),
        case Port of
          undefined -> ok;
          _ ->
            file:write(ChannelsFile, ["port=", integer_to_list(Port), "\n"])
        end,
        file:write(ChannelsFile, ["domain=", Host, "\n"]),
        file:write(ChannelsFile, ["fromdomain=", Host, "\n"]),
        file:write(ChannelsFile, "type=user\n"),
        file:write(ChannelsFile, "\n"),

        R3 = lists:foldl(fun (IP, R2) ->
          dict:append({util:to_string(IP), Number}, Channel#channel.id, R2)
        end, R1, IPs),
        {R3, I + 1}
      end, {ChannelIndex, 0}, Expanded),

      NewRegistryIndex = case channel:register(Channel) of
        true ->
          file:write(RegFile, ["register => ", Username, ":", Password, "@", Domain, "/", Number, "\n"]),
          dict:store({Username, Domain}, Channel#channel.id, RegistryIndex);
        _ ->
          RegistryIndex
      end,

      generate_config(Rest, RegFile, ChannelsFile, NewCache, NewChannelIndex, NewRegistryIndex)
  end.

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
      lists:foldl(fun({_, _, Port, Host}, Out) ->
        New = case inet_res:gethostbyname(Host) of
          {ok, #hostent{h_addr_list = IpList}} ->
            IPs = map_ips(IpList),
            {Host, IPs, Port};
          _ -> {Host, [], Port}
        end,
        [New | Out]
      end, [], AddrList);
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

