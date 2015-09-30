-module(twilio_httpd_module).
-export([do/1]).

-include_lib("inets/include/httpd.hrl").
-include("db.hrl").

do(#mod{request_uri = RequestUri, method = "POST", entity_body = Body}) ->
  Params = uri:parse_qs(Body),
  QSParams = uri:parse_qs(RequestUri),
  CallSid = proplists:get_value("CallSid", Params),

  ResponseBody = case twilio_pbx:find(CallSid) of
    undefined ->
      Pbx = twilio_pbx:new(CallSid),
      case proplists:get_value("VerboiceSid", QSParams) of
        undefined ->
          AccountSid = proplists:get_value("AccountSid", Params),
          Number = util:normalize_phone_number(proplists:get_value("To", Params)),
          CallerId = util:normalize_phone_number(proplists:get_value("From", Params)),
          Channel = twilio_channel_srv:find_channel(AccountSid, Number),

          {ok, SessionPid} = session:new(),

          % FIX this may lead to a race condition if the session reaches a flush before the resume is called
          session:answer(SessionPid, Pbx, Channel#channel.id, CallerId);
        SessionId ->
          SessionPid = session:find(SessionId), %TODO: return error if session was not found
          session:answer(SessionPid, Pbx)
      end,

      Pbx:resume(Params);
    Pbx ->
      CallStatus = proplists:get_value("CallStatus", Params),
      case CallStatus of
        "completed" -> Pbx:user_hangup(), "OK";
        _ -> Pbx:resume(Params)
      end
  end,

  Response = [{response, {200, ResponseBody}}],
  {proceed, Response};

do(ModData) ->
  io:format("Unhandled: ~p~n", [ModData]),
  {proceed, [ModData]}.
