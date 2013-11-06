-module(twilio_api).
-export([incoming_phone_numbers/1, incoming_phone_numbers/2]).

-include("uri.hrl").

incoming_phone_numbers(Channel) ->
  incoming_phone_numbers(Channel, util:normalize_phone_number(channel:number(Channel))).

incoming_phone_numbers(Channel, PhoneNumber) ->
  AccountSid = channel:account_sid(Channel),
  AuthToken = channel:auth_token(Channel),
  RequestUrl = "https://api.twilio.com/2010-04-01/Accounts/" ++ AccountSid ++ "/IncomingPhoneNumbers.json",
  RequestUri = uri:parse(RequestUrl),
  case uri:get([{basic_auth, {AccountSid, AuthToken}}], RequestUri#uri{query_string = [{'PhoneNumber', PhoneNumber}]}) of
    {ok, {{_, 200, _}, _, Body}} -> {ok, {Data}} = json:decode(Body), {ok, Data};
    {ok, {{_, _, Status}, _, _}} -> {error, Status};
    {error, Reason} -> {error, Reason}
  end.


