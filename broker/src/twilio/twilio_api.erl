-module(twilio_api).
-export([incoming_phone_numbers/1, incoming_phone_numbers/2, update_voice_url/2]).

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

update_voice_url(Channel, PhoneNumber) ->
  % To update a phone numbe we first need to get its SID
  case incoming_phone_number_sid(Channel, PhoneNumber) of
    {ok, PhoneSid} ->
      AccountSid = channel:account_sid(Channel),
      AuthToken = channel:auth_token(Channel),
      RequestUrl = "https://api.twilio.com/2010-04-01/Accounts/" ++ AccountSid ++ "/IncomingPhoneNumbers/" ++ binary_to_list(PhoneSid) ++ ".json",
      {ok, TwilioCallbackUrl} = application:get_env(twilio_callback_url),
      RequestBody = [{'VoiceUrl', list_to_binary(TwilioCallbackUrl)}],
      uri:post_form(RequestBody, [{basic_auth, {AccountSid, AuthToken}}], RequestUrl),
      ok;
    _Error ->
      % For now ignore the error: the channel will appear as errored
      % anway in the UI because of twilio_channel_srv:check_status.
      ok
  end.

incoming_phone_number_sid(Channel, PhoneNumber) ->
  case incoming_phone_numbers(Channel, PhoneNumber) of
    {ok, Data} ->
      case proplists:get_value(<<"incoming_phone_numbers">>, Data) of
        [{IncomingPhoneNumber}] ->
          PhoneSid = proplists:get_value(<<"sid">>, IncomingPhoneNumber),
          {ok, PhoneSid};
        [] -> {error, twilio_number_not_found}
      end;
    Error -> Error
  end.
