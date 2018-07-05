-module(africas_talking_httpd_module).
-export([do/1]).

-include_lib("inets/include/httpd.hrl").
-include("db.hrl").
-compile([{parse_transform, lager_transform}]).

do(#mod{request_uri = "/africas_talking/" ++ _, method = "POST", entity_body = Body, data = Data}) ->
  Params = uri:parse_qs(Body),
  case proplists:get_value("callSessionState", Params) of
    undefined ->
      httpd_utils:if_not_already_handled(Data, fun() ->
        CallSid = proplists:get_value("sessionId", Params),

        ResponseBody = case africas_talking_pbx:find(CallSid) of
          undefined ->
            Pbx = africas_talking_pbx:new(CallSid),
            case africas_talking_sid:find_session_pid(CallSid) of
              undefined ->
                Number = util:normalize_phone_number(proplists:get_value("destinationNumber", Params)),
                CallerId = util:normalize_phone_number(proplists:get_value("callerNumber", Params)),
                % The destinationNumber only is used to fetch the channel
                Channel = africas_talking_channel_srv:find_channel(Number),

                {ok, SessionPid} = session:new(),
                africas_talking_sid:start(CallSid, SessionPid),

                % FIX this may lead to a race condition if the session reaches a flush before the resume is called
                session:answer(SessionPid, Pbx, Channel#channel.id, CallerId);
              SessionPid ->
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
        {proceed, Response}
      end);
    _ ->
      lager:info("Receiving callSessionState ~p", [Params]),
      {proceed, [{response, {200, "OK"}}]}
  end;

do(ModData) ->
  io:format("Unhandled: ~p~n", [ModData]),
  {proceed, [ModData]}.
