-module(channel).
-export([find_all_sip/0, domain/1, number/1]).

-include("db.hrl").

find_all_sip() ->
  Rows = db:select("SELECT id, account_id, call_flow_id, name, config, type FROM channels " ++
    "WHERE type IN ('Channels::Sip', 'Channels::CustomSip', 'Channels::TemplateBasedSip')"),
  lists:map(fun([Id, AccountId, CallFlowId, Name, Config, Type]) ->
    {ok, [RealConfig]} = yaml:load(Config),
    #channel{
      id = Id,
      account_id = AccountId,
      call_flow_id = CallFlowId,
      name = Name,
      config = RealConfig,
      type = Type
    }
  end, Rows).

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