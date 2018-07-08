-module(africas_talking_httpd_module).
-export([do/1]).

-include_lib("inets/include/httpd.hrl").
-include("db.hrl").
-compile([{parse_transform, lager_transform}]).

do(#mod{request_uri = "/africas_talking/" ++ _, method = "POST", entity_body = Body, data = Data}) ->
  Params = uri:parse_qs(Body),
  CallSid = proplists:get_value("sessionId", Params),
  Pbx = africas_talking_pbx:find(CallSid),
  case proplists:get_value("callSessionState", Params) of
    undefined ->
      httpd_utils:if_not_already_handled(Data, fun() ->
        ResponseBody = case Pbx of
          undefined ->
            NewPbx = africas_talking_pbx:new(CallSid),
            case africas_talking_sid:find_session_pid(CallSid) of
              undefined ->
                Number = util:normalize_phone_number(proplists:get_value("destinationNumber", Params)),
                CallerId = util:normalize_phone_number(proplists:get_value("callerNumber", Params)),
                % Channel is fetched by using just the destinationNumber (Inbound call)
                Channel = africas_talking_channel_srv:find_channel(Number),

                {ok, SessionPid} = session:new(),
                africas_talking_sid:start(CallSid, SessionPid),

                % FIX this may lead to a race condition if the session reaches a flush before the resume is called
                session:answer(SessionPid, NewPbx, Channel#channel.id, CallerId);
              SessionPid ->
                session:answer(SessionPid, NewPbx)
            end,

            NewPbx:resume(Params);
          FoundPbx ->
            FoundPbx:resume(Params)
          end,
          Response = [{response, {200, ResponseBody}}],
          {proceed, Response}
        end);
    % TODO. If the call fails and the session is still active, a user_hangup is logged,
    % which is not true. Handle those cases properly.
    "Completed" ->
      case Pbx of
        undefined ->
          ok;
        FoundPbx ->
          FoundPbx:user_hangup()
      end,
      {proceed, [{response, {200, "OK"}}]};
    _ ->
      lager:info("Receiving callSessionState ~p", [Params]),
      {proceed, [{response, {200, "OK"}}]}
  end;

do(ModData) ->
  io:format("Unhandled: ~p~n", [ModData]),
  {proceed, [ModData]}.
