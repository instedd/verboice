-module(channel).
-export([find_all_sip/0, domain/1, number/1]).
-define(TABLE_NAME, "channels").

-define(MAP(Channel),
  {ok, [Config]} = yaml:load(Channel#channel.config),
  Channel#channel{config = Config}
  ).

-include("model.hrl").

find_all_sip() ->
  find_all({type, in, ["Channels::Sip", "Channels::CustomSip", "Channels::TemplateBasedSip"]}).

domain(Channel = #channel{type = <<"Channels::TemplateBasedSip">>}) ->
  case proplists:get_value(<<"kind">>, Channel#channel.config) of
    % TODO: Load template domains from yaml file
    <<"Callcentric">> -> "callcentric.com";
    <<"Skype">> -> "sip.skype.com"
  end;

domain(#channel{config = Config}) ->
  proplists:get_value(<<"domain">>, Config).

number(#channel{config = Config}) ->
  binary_to_list(proplists:get_value(<<"number">>, Config)).