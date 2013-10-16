-module(channel).
-export([find_all_sip/0, find_all_twilio/0, domain/1, number/1, limit/1, broker/1, username/1, password/1, is_outbound/1, register/1]).
-export([account_sid/1, auth_token/1]).
-define(CACHE, true).
-define(TABLE_NAME, "channels").

-define(MAP(Channel),
  {ok, [Config]} = yaml:load(Channel#channel.config, [{schema, yaml_schema_ruby}]),
  Channel#channel{config = Config}
).

-include_lib("erl_dbmodel/include/model.hrl").

find_all_sip() ->
  find_all({type, in, ["Channels::Sip", "Channels::CustomSip", "Channels::TemplateBasedSip"]}).

find_all_twilio() ->
  find_all({type, "Channels::Twilio"}).

domain(Channel = #channel{type = <<"Channels::TemplateBasedSip">>}) ->
  case proplists:get_value("kind", Channel#channel.config) of
    % TODO: Load template domains from yaml file
    "Callcentric" -> "callcentric.com";
    "Skype" -> "sip.skype.com"
  end;

domain(#channel{config = Config}) ->
  proplists:get_value("domain", Config, <<>>).

number(#channel{config = Config}) ->
  util:to_string(proplists:get_value("number", Config, <<>>)).

username(#channel{config = Config}) ->
  proplists:get_value("username", Config).

password(#channel{config = Config}) ->
  proplists:get_value("password", Config).

account_sid(#channel{config = Config}) ->
  proplists:get_value("account_sid", Config).

auth_token(#channel{config = Config}) ->
  proplists:get_value("auth_token", Config).

is_outbound(#channel{type = <<"Channels::TemplateBasedSip">>}) ->
  true;

is_outbound(#channel{config = Config}) ->
  case proplists:get_value("direction", Config) of
    "outbound" -> true;
    "both" -> true;
    _ -> false
  end.

register(#channel{type = <<"Channels::TemplateBasedSip">>}) ->
  true;

register(#channel{config = Config}) ->
  case proplists:get_value("register", Config) of
    "true" -> true;
    "1" -> true;
    _ -> false
  end.

limit(#channel{config = Config}) ->
  case proplists:get_value("limit", Config) of
    [] -> 1;
    List when is_list(List) -> list_to_integer(List);
    Int when is_integer(Int) -> Int;
    _ -> 1
  end.

broker(#channel{type = <<"Channels::Twilio">>}) -> twilio_broker;
broker(_) -> asterisk_broker.
