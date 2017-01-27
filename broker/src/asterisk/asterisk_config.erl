-module(asterisk_config).
-export([generate/1, expand_domain/1]).

-include_lib("kernel/include/inet.hrl").
-include("db.hrl").

generate(ConfigFilePath) ->
  {ok, ConfigFile} = file:open(ConfigFilePath, [write]),
  try generate_config(ConfigFile) of
    R -> R
  after
    file:close(ConfigFile)
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
generate_config(ConfigFile) ->
  Channels = channel:find_all_sip(),
  generate_config(Channels, ConfigFile, dict:new(), dict:new(), dict:new(), []).

generate_config([], _, _, ChannelIndex, RegistryIndex, ConfigErrors) -> {ChannelIndex, RegistryIndex, ConfigErrors};
generate_config([Channel | Rest], ConfigFile, ResolvCache, ChannelIndex, RegistryIndex, ConfigErrors) ->
  HeaderLines = binary:split(erlang:iolist_to_binary(pretty_print(Channel)), <<"\n">>, [global]),
  lists:foreach(fun(Line) ->
    file:write(ConfigFile, ["; ", Line, "\n"])
  end, HeaderLines),
  file:write(ConfigFile, "\n"),

  ChannelId = Channel#channel.id,
  Section = ["verboice_", integer_to_list(ChannelId)],
  Username = channel:username(Channel),
  Password = channel:password(Channel),
  Domain = channel:domain(Channel),
  Number = channel:number(Channel),

  case Domain of
    [] ->
      % Endpoint
      file:write(ConfigFile, ["[", Section, "]\n"]),
      file:write(ConfigFile, "type=endpoint\n"),
      file:write(ConfigFile, "context=verboice\n"),
      file:write(ConfigFile, ["aors=", Section, "\n"]),
      file:write(ConfigFile, ["auth=", Section, "\n"]),
      file:write(ConfigFile, "disallow=all\n"),
      file:write(ConfigFile, "allow=ulaw\n"),
      file:write(ConfigFile, "allow=gsm\n"),
      file:write(ConfigFile, "rtp_symmetric=yes\n"),
      file:write(ConfigFile, "direct_media=no\n"),
      file:write(ConfigFile, "rewrite_contact=yes\n"),
      file:write(ConfigFile, "identify_by=auth_username\n"),
      file:write(ConfigFile, "\n"),

      % Auth
      file:write(ConfigFile, ["[", Section, "]\n"]),
      file:write(ConfigFile, "type=auth\n"),
      file:write(ConfigFile, "auth_type=userpass\n"),
      file:write(ConfigFile, ["username=", Section, "\n"]),
      file:write(ConfigFile, ["password=", Password, "\n"]),
      file:write(ConfigFile, "\n"),

      % AOR
      file:write(ConfigFile, ["[", Section, "]\n"]),
      file:write(ConfigFile, "type=aor\n"),
      file:write(ConfigFile, "qualify_frequency=60\n"),
      file:write(ConfigFile, "max_contacts=1\n"),
      file:write(ConfigFile, "remove_existing=yes\n"),
      file:write(ConfigFile, "\n"),

      generate_config(Rest, ConfigFile, ResolvCache, ChannelIndex, RegistryIndex, ConfigErrors);

    _ ->
      {Expanded, NewCache} = expand_domain(Domain, ResolvCache),
      % TODO: if we consider the channel's direction (ie. inbound/outbound) we
      % may relax this condition since we only need a set of resolved IP
      % addresses to match incoming calls. Although, the domain needs to be
      % resolvable also if registration is enabled.
      case is_resolved(Expanded) of
        true ->
          HasAuth = length(Username) > 0 andalso length(Password) > 0,
          UserOrNumber = case length(Username) > 0 of
            true -> Username;
            _ -> Number
          end,

          % Registration
          NewRegistryIndex = case channel:register(Channel) of
            true ->
              file:write(ConfigFile, ["[", Section, "]\n"]),
              file:write(ConfigFile, "type=registration\n"),
              if HasAuth ->
                file:write(ConfigFile, ["outbound_auth=", Section, "\n"]);
                true -> ok
              end,
              file:write(ConfigFile, ["server_uri=sip:", Domain, "\n"]),
              file:write(ConfigFile, ["client_uri=sip:", UserOrNumber, "@", Domain, "\n"]),
              %% When Asterisk Version > 13.1...
              % file:write(ConfigFile, "line=yes\n"),
              % file:write(ConfigFile, ["endpoint=", Section, "\n"]),
              file:write(ConfigFile, "\n"),
              dict:store({Username, Domain}, Channel#channel.id, RegistryIndex);

            _ -> RegistryIndex
          end,

          % Endpoint
          file:write(ConfigFile, ["[", Section, "]\n"]),
          file:write(ConfigFile, "type=endpoint\n"),
          file:write(ConfigFile, "context=verboice\n"),
          file:write(ConfigFile, ["aors=", Section, "\n"]),
          if HasAuth ->
            file:write(ConfigFile, ["outbound_auth=", Section, "\n"]);
            true -> ok
          end,
          file:write(ConfigFile, ["from_user=", UserOrNumber, "\n"]),
          file:write(ConfigFile, ["from_domain=", Domain, "\n"]),
          file:write(ConfigFile, "disallow=all\n"),
          file:write(ConfigFile, "allow=ulaw\n"),
          file:write(ConfigFile, "allow=gsm\n"),
          file:write(ConfigFile, "\n"),

          % Auth
          if HasAuth ->
            file:write(ConfigFile, ["[", Section, "]\n"]),
            file:write(ConfigFile, "type=auth\n"),
            file:write(ConfigFile, "auth_type=userpass\n"),
            file:write(ConfigFile, ["username=", Username, "\n"]),
            file:write(ConfigFile, ["password=", Password, "\n"]),
            file:write(ConfigFile, "\n");
            true -> ok
          end,

          % AOR
          file:write(ConfigFile, ["[", Section, "]\n"]),
          file:write(ConfigFile, "type=aor\n"),
          file:write(ConfigFile, "qualify_frequency=60\n"),
          file:write(ConfigFile, ["contact=sip:", Domain, "\n"]),
          file:write(ConfigFile, "\n"),

          % Identify
          file:write(ConfigFile, ["[", Section, "]\n"]),
          file:write(ConfigFile, "type=identify\n"),
          file:write(ConfigFile, ["endpoint=", Section, "\n"]),

          {NewChannelIndex, _} = lists:foldl(fun ({_Host, IPs, Port}, {R1, I}) ->
            case Port of
              undefined -> ok;
              _ -> ok
                % file:write(ConfigFile, ["port=", integer_to_list(Port), "\n"])
            end,

            R3 = lists:foldl(fun (IP, R2) ->
              file:write(ConfigFile, ["match=", IP, "\n"]),
              dict:append({util:to_string(IP), Number}, Channel#channel.id, R2)
            end, R1, IPs),
            {R3, I + 1}
          end, {ChannelIndex, 0}, Expanded),
          file:write(ConfigFile, "\n"),

          generate_config(Rest, ConfigFile, NewCache, NewChannelIndex, NewRegistryIndex, ConfigErrors);

        false ->
          % Domain could not be resolved, we cannot generate the endpoint
          % information for Asterisk
          Message = iolist_to_binary(["Error: could not resolve domain ", Domain]),
          NewConfigErrors = [{ChannelId, false, [Message]}|ConfigErrors],
          generate_config(Rest, ConfigFile, NewCache, ChannelIndex, RegistryIndex, NewConfigErrors)
      end
  end.

is_resolved([]) ->
  false;
is_resolved([{_Host, [_Ip|_], _Port}|_]) ->
  true;
is_resolved([_|Rest]) ->
  is_resolved(Rest).

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
        _ -> [{Domain, [], undefined}]
      end
  end.

is_ip(Address) ->
  case re:run(Address, "^(\\d+\\.){3}\\d+$") of
    {match, _} -> true;
    _ -> false
  end.

map_ips(IpList) ->
  [iolist_to_binary(io_lib:format("~B.~B.~B.~B", [A,B,C,D])) || {A,B,C,D} <- IpList].


pretty_print(Channel) ->
  io_lib_pretty:print(Channel, fun pretty_print/2).

pretty_print(channel, N) ->
  N = record_info(size, channel) - 1,
  record_info(fields, channel);
pretty_print(_, _) ->
  no.
