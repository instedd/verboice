-module(twilio_broker).
-export([start_link/0, init/0, dispatch/1]).

-include("session.hrl").

start_link() ->
  broker:start_link(?MODULE).

init() ->
  ok.

dispatch(_Session = #session{session_id = SessionId, channel = Channel, address = Address}) ->
  io:format("Channel ~p~n", [Channel]),
  AccountSid = channel:account_sid(Channel),
  AuthToken = channel:auth_token(Channel),
  RequestUrl = "https://api.twilio.com/2010-04-01/Accounts/" ++ AccountSid ++ "/Calls",
  Headers = [{"Authorization", "Basic " ++ base64:encode_to_string(lists:append([AccountSid,":",AuthToken]))}],
  QueryString = uri:format_qs([
    {"From", util:normalize_phone_number(channel:number(Channel))},
    {"To", util:normalize_phone_number(Address)},
    {"Url", "http://manas.no-ip.biz:8080?VerboiceSid=" ++ http_uri:encode(SessionId)}
  ]),

  Response = httpc:request(post, {RequestUrl, Headers, "application/x-www-form-urlencoded", QueryString}, [], []),
  io:format("Response: ~p~n", [Response]),
  case Response of
    {ok, {{_, 201, _}, _, _}} -> ok;
    {ok, {{_, _, Reason}, _, _}} -> {error, Reason};
    _ ->
      timer:apply_after(timer:minutes(1), broker, notify_ready, [?MODULE]),
      {error, unavailable}
  end.
