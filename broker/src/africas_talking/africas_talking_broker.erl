-module(africas_talking_broker).
-export([start_link/0, init/0, dispatch/1, create_channel/1, destroy_channel/1, get_channel_status/1, parse_exception/2]).

-behaviour(broker).
-include("session.hrl").
-include("uri.hrl").

-include_lib("xmerl/include/xmerl.hrl").

-compile([{parse_transform, lager_transform}]).

start_link() ->
  broker:start_link(?MODULE).

get_channel_status(ChannelIds) ->
  africas_talking_channel_srv:get_channel_status(ChannelIds).

init() ->
  ok.

create_channel(Id) -> africas_talking_channel_srv:channel_updated(Id), ok.

destroy_channel(Id) -> africas_talking_channel_srv:channel_destroyed(Id), ok.

dispatch(_Session = #session{session_id = SessionId, channel = Channel, address = Address}) ->
  ApiKey = channel:api_key(Channel),
  Username = channel:username(Channel),

  RequestUrl = ["https://voice.sandbox.africastalking.com/call"],
  RequestBody = [
    {'from', iolist_to_binary(channel:number(Channel))},
    {'to', iolist_to_binary(Address)},
    {'username', iolist_to_binary(Username)}
  ],

  Response = uri:post_form(RequestBody, [{api_key, ApiKey}, {accept, "application/json"}], RequestUrl),
  case Response of
    {ok, {{_, 200, _}, _, RawResponse}} ->
      {ok, {JSONResponse}} = json:decode(RawResponse),
      [{Entries}] = proplists:get_value(<<"entries">>, JSONResponse),
      AfricasTalkingId = proplists:get_value(<<"sessionId">>, Entries),
      africas_talking_sid:start(AfricasTalkingId, session:find(SessionId)),
      ok;
    {ok, {{_, _, Reason}, _, Msg}} -> parse_exception(Reason, Msg);
    _ ->
      timer:apply_after(timer:minutes(1), broker, notify_ready, [?MODULE]),
      {error, unavailable}
  end.

% TODO. Parse exception in Africa's Talking
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
