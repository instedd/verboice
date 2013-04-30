-module(twilio_httpd_module).
-export([do/1]).

-include_lib("inets/include/httpd.hrl").
-include("db.hrl").

do(#mod{absolute_uri = AbsoluteUri, request_uri = "/", method = "POST", entity_body = Body}) ->
  Params = util:parse_qs(Body),
  CallSid = proplists:get_value("CallSid", Params),

  ResponseBody = case twilio_pbx:find(CallSid) of
    undefined ->
      AccountSid = list_to_binary(proplists:get_value("AccountSid", Params)),
      Number = util:normalize_phone_number(proplists:get_value("To", Params)),
      Channel = find_channel(AccountSid, Number),
      CallbackUrl = "http://" ++ AbsoluteUri,

      Pbx = twilio_pbx:new(CallSid, CallbackUrl),
      {ok, SessionPid} = session:new(),
      % FIX this may lead to a race condition if the session reaches a flush before the resume is called
      session:answer(SessionPid, Pbx, Channel#channel.id),
      Pbx:resume(Params);
    Pbx ->
      CallStatus = proplists:get_value("CallStatus", Params),
      case CallStatus of
        "completed" -> Pbx:terminate(), "OK";
        _ -> Pbx:resume(Params)
      end
  end,

  Response = [{response, {200, ResponseBody}}],
  {proceed, Response};

do(_) ->
  {proceed, [{response, {404, "Not Found"}}]}.

find_channel(AccountSid, Number) ->
  Channels = channel:find_all_twilio(),
  find_channel(Channels, AccountSid, Number).

find_channel([], _, _) -> undefined;
find_channel([Channel = #channel{config = Config} | Rest], AccountSid, Number) ->
  ChannelAccountSid = proplists:get_value(<<"account_sid">>, Config),
  ChannelNumber = util:normalize_phone_number(proplists:get_value(<<"number">>, Config)),
  if
    (AccountSid == ChannelAccountSid) and (Number == ChannelNumber) ->
      Channel;
    true ->
      find_channel(Rest, AccountSid, Number)
  end.