-module(channel).
-compile([{parse_transform, lager_transform}]).
-export([find_all_sip/0, find_all_twilio/0, find_all_africas_talking/0,
         domain/1, number/1, username/1, password/1,
         broker/1, is_outbound/1, limit/1, register/1,
         log_broken_channels/2,
         disable_by_id/1, disable_by_ids/1,
         account_sid/1, auth_token/1, api_key/1, base_url/1]).

-define(CACHE, true).
-define(TABLE_NAME, "channels").
-define(MAP, [{config, yaml_serializer}]).
-include_lib("erl_dbmodel/include/model.hrl").

find_all_sip() ->
  find_all([{enabled, 1}, {type, in, ["Channels::Sip", "Channels::CustomSip", "Channels::TemplateBasedSip", "Channels::SipServer"]}]).

find_all_twilio() ->
  find_all([{enabled, 1}, {type, "Channels::Twilio"}]).

find_all_africas_talking() ->
  find_all([{enabled, 1}, {type, "Channels::AfricasTalking"}]).

domain(Channel = #channel{type = <<"Channels::TemplateBasedSip">>}) ->
  case proplists:get_value("kind", Channel#channel.config) of
    % Keep in sync with config/sip_channel_templates.yml
    "Callcentric" -> "callcentric.com";
    "Skype" -> "sip.skype.com";
    "Nexmo" -> "sip.nexmo.com"
  end;

domain(#channel{config = Config}) ->
  proplists:get_value("domain", Config, []).

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

api_key(#channel{config = Config}) ->
  proplists:get_value("api_key", Config).

base_url(#channel{type = <<"Channels::Twilio">>, config = Config}) ->
  BaseUrl = proplists:get_value("base_url", Config),
  case io_lib:char_list(BaseUrl) of
    true ->
      case string:strip(BaseUrl) of
        "" -> verboice_config:twilio_base_url();
        _ -> BaseUrl
      end;
    false ->
      verboice_config:twilio_base_url()
  end;

base_url(_) -> nil.

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
broker(#channel{type = <<"Channels::AfricasTalking">>}) -> africas_talking_broker;
broker(_) -> asterisk_broker.

log_broken_channels(PrevStatus, NewStatus) ->
  dict:fold(fun
    (ChannelId, {_, false, _}, _) ->
      case channel_was_disconnected(ChannelId, PrevStatus) of
        false ->
          Channel = channel:find(ChannelId),
          lager:error("Channel ~s (id: ~p) is down", [Channel#channel.name, ChannelId]), ok;
        _ -> ok
      end;
    (_, _, _) -> ok
  end, undefined, NewStatus).

channel_was_disconnected(_, undefined) -> false;
channel_was_disconnected(ChannelId, Status) ->
  case dict:find(ChannelId, Status) of
    {ok, {_, false, _}} -> true;
    _ -> false
  end.

disable_by_id(ChannelId) ->
  case channel:find(ChannelId) of
    Channel = #channel{} ->
      lager:info("Disabling channel ~s (id: ~B)", [Channel#channel.name, ChannelId]),
      % Keep in sync with channel.rb
      %   self.enabled = false
      %   save!
      %   queued_calls.each do |call|
      %     call.cancel_call!
      %   end
      %   queued_calls.destroy_all

      db:update(["UPDATE channels SET enabled = 0 WHERE id = ",
                 mysql:encode(ChannelId)]),
      db:update(["UPDATE call_logs SET state = 'cancelled' WHERE id IN ",
                 "(SELECT call_log_id FROM queued_calls WHERE channel_id = ",
                 mysql:encode(ChannelId), ")"]),
      db:update(["DELETE FROM queued_calls WHERE channel_id = ",
                 mysql:encode(ChannelId)]),
      ok;
    _ ->
      % channel not found
      ok
  end.

disable_by_ids(ChannelIds) ->
  lists:foreach(fun disable_by_id/1, ChannelIds).
