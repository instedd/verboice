-module(twilio_broker).
-export([start_link/0, init/0, dispatch/1, create_channel/1, destroy_channel/1, get_channel_status/1]).

-behaviour(broker).
-include("session.hrl").
-include("uri.hrl").

start_link() ->
  broker:start_link(?MODULE).

get_channel_status(ChannelIds) ->
  twilio_channel_srv:get_channel_status(ChannelIds).

init() ->
  ok.

create_channel(_Id) -> twilio_channel_srv:reload_channels(), ok.

destroy_channel(_Id) -> twilio_channel_srv:reload_channels(), ok.

dispatch(_Session = #session{session_id = SessionId, channel = Channel, address = Address}) ->
  AccountSid = channel:account_sid(Channel),
  AuthToken = channel:auth_token(Channel),
  {ok, CallbackUrl} = application:get_env(twilio_callback_url),
  CallbackUri = uri:parse(CallbackUrl),

  RequestUrl = ["https://api.twilio.com/2010-04-01/Accounts/", AccountSid, "/Calls"],
  RequestBody = [
    {'From', util:normalize_phone_number(channel:number(Channel))},
    {'To', util:normalize_phone_number(Address)},
    {'Url', uri:format(CallbackUri#uri{query_string = [{'VerboiceSid', SessionId}]})}
  ],

  Response = uri:post_form(RequestBody, [{basic_auth, {AccountSid, AuthToken}}], RequestUrl),
  io:format("Response: ~p~n", [Response]),
  case Response of
    {ok, {{_, 201, _}, _, _}} -> ok;
    {ok, {{_, _, Reason}, _, _}} -> {error, Reason};
    _ ->
      timer:apply_after(timer:minutes(1), broker, notify_ready, [?MODULE]),
      {error, unavailable}
  end.
