-module(nuntium).
-export([run/2, can_play/1]).
-include("session.hrl").
-include("db.hrl").
-include("uri.hrl").

run(Args, Session = #session{project = Project}) ->
  Guid = proplists:get_value(resource_guid, Args),
  RcptType = proplists:get_value(rcpt_type, Args),
  Expr = proplists:get_value(expr, Args),

  {Result, Message} = case rcpt_address(RcptType, Expr, Session) of
    undefined -> {error, "Missing recipient"};
    Address ->
      case resource:prepare(Guid, Session#session{pbx = nuntium}) of
        {text, _Lang, Body} ->
          NuntiumArgs = [
            {from, <<"sms://verboice">>},
            {to, Address},
            {body, Body},
            {account_id, Project#project.account_id}
          ],
          poirot:log(debug, "Sending to nuntium: ~p", [NuntiumArgs]),
          case nuntium_api:send_ao(NuntiumArgs) of
            ok -> {info, "Sent"};
            {error, Reason} -> {error, Reason}
          end;
        _ -> {error, "Missing text to send"}
      end
  end,

  case Result of
    info -> poirot:log(info, Message);
    error -> poirot:log(error, Message)
  end,

  {next, Session}.

rcpt_address(RcptType, Expr, Session) ->
  case rcpt_address_from_session(RcptType, Expr, Session) of
    undefined -> undefined;
    "" -> undefined;
    Address ->
      case binary:match(Address, <<"://">>) of
        nomatch -> <<"sms://", Address/binary>>;
        _ -> Address
      end
  end.

rcpt_address_from_session(caller, _, #session{js_context = JS, address = Address}) ->
  case erjs_context:get(var_sms_number, JS) of
    undefined -> Address;
    "" -> Address;
    Number -> list_to_binary(Number)
  end;

rcpt_address_from_session(expr, Expr, #session{js_context = JS}) ->
  {Value, _} = erjs:eval(Expr, JS),
  case Value of
    null -> undefined;
    _ -> list_to_binary(util:to_string(Value))
  end.

can_play({text, _}) -> true;
can_play(_) -> false.
