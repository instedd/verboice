-module(africas_talking_httpd_module).
-export([do/1]).

-include_lib("inets/include/httpd.hrl").
-include("db.hrl").
-compile([{parse_transform, lager_transform}]).

do(#mod{request_uri = "/africas_talking/" ++ _ = RequestUri, method = "POST", entity_body = Body}) ->
  Params = uri:parse_qs(Body),
  QSParams = uri:parse_qs(RequestUri),

  CallSid = proplists:get_value("sessionId", Params),

  ResponseBody = case africas_talking_pbx:find(CallSid) of
    undefined ->
      Pbx = africas_talking_pbx:new(CallSid),
      case proplists:get_value("VerboiceSid", QSParams) of
        undefined ->
          Number = util:normalize_phone_number(proplists:get_value("destinationNumber", Params)),
          CallerId = util:normalize_phone_number(proplists:get_value("callerNumber", Params)),
          % The destinationNumber only is used to fetch the channel
          Channel = africas_talking_channel_srv:find_channel(Number),

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
