-module(twilio_broker).
-export([start_link/0, init/0, dispatch/1, create_channel/1, destroy_channel/1, get_channel_status/1, parse_exception/2]).

-behaviour(broker).
-include("session.hrl").
-include("uri.hrl").

-include_lib("xmerl/include/xmerl.hrl").

-compile([{parse_transform, lager_transform}]).

start_link() ->
  broker:start_link(?MODULE).

get_channel_status(ChannelIds) ->
  twilio_channel_srv:get_channel_status(ChannelIds).

init() ->
  ok.

create_channel(Id) -> twilio_channel_srv:channel_updated(Id), ok.

destroy_channel(Id) -> twilio_channel_srv:channel_destroyed(Id), ok.

dispatch(_Session = #session{session_id = SessionId, channel = Channel, address = Address}) ->
  AccountSid = channel:account_sid(Channel),
  AuthToken = channel:auth_token(Channel),
  CallbackUrl = verboice_config:twilio_callback_url(),
  CallbackUri = uri:parse(CallbackUrl),

  RequestUrl = [channel:base_url(Channel) ++ "/Accounts/", AccountSid, "/Calls"],
  RequestBody = [
    {'From', util:normalize_phone_number(channel:number(Channel))},
    {'To', util:normalize_phone_number(Address)},
    {'Url', uri:format(CallbackUri#uri{query_string = [{'VerboiceSid', SessionId}]})}
  ],

  Response = uri:post_form(RequestBody, [{basic_auth, {AccountSid, AuthToken}}], RequestUrl),
  lager:info("Twilio response: ~p~n", [Response]),
  case Response of
    {ok, {{_, 201, _}, _, _}} -> ok;
    {ok, {{_, _, Reason}, _, Msg}} -> parse_exception(Reason, Msg);
    _ ->
      timer:apply_after(timer:minutes(1), broker, notify_ready, [?MODULE]),
      {error, unavailable}
  end.

parse_exception(Reason, Body) ->
  try
    {Doc, []} = xmerl_scan:string(Body),
    [Exception] = xmerl_xs:select("/TwilioResponse/RestException", Doc),
    FullCode = case xmerl_xs:value_of(xmerl_xs:select("./Code", Exception)) of
      [Code] -> "twilio:" ++ Code;
      _ -> undefined
    end,
    [Message] = xmerl_xs:value_of(xmerl_xs:select("./Message", Exception)),
    {error, Message, FullCode}
  catch
    _ -> {error, Reason}
  end.
