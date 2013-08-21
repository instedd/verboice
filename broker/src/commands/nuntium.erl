-module(nuntium).
-export([run/2, can_play/1]).
-include("session.hrl").
-include("db.hrl").
-include("uri.hrl").

run(Args, Session = #session{call_log = CallLog, project = Project}) ->
  Guid = proplists:get_value(resouce_guid, Args),
  RcptType = proplists:get_value(rcpt_type, Args),
  Expr = proplists:get_value(expr, Args),

  CallLog:info(["Send text message '", Guid, "'"], [{command, "nuntium"}, {action, "start"}]),
  Result = case rcpt_address(RcptType, Expr, Session) of
    undefined -> "Missing recipient";
    Address ->
      case resource:prepare(Guid, Session#session{pbx = nuntium}) of
        {text, _Lang, Body} ->
          send_message(<<"sms://verboice">>, Address, Body, Project#project.account_id);
        _ -> "Missing text to send"
      end
  end,

  CallLog:info(["Result:", Result], []),

  {next, Session}.

send_message(From, To, Body, AccountId) ->
  {ok, Host} = application:get_env(nuntium_host),
  {ok, Account} = application:get_env(nuntium_account),
  {ok, App} = application:get_env(nuntium_app),
  {ok, AppPassword} = application:get_env(nuntium_app_password),

  Uri = uri:parse(Host),
  MsgUri = Uri#uri{path = [$/, Account, $/, App, "/send_ao"], query_string = [
    {from, From}, {to, To}, {body, Body}, {account_id, AccountId}
  ]},
  Response = MsgUri:get([{basic_auth, {[Account, $/, App], AppPassword}}]),
  io:format("~p~n", [Response]),
  uri:format(MsgUri).

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
  case erjs_object:get(var_sms_number, JS) of
    undefined -> Address;
    "" -> Address;
    Number -> Number
  end;

rcpt_address_from_session(expr, Expr, #session{js_context = JS}) ->
  {Value, _} = erjs:eval(Expr, JS),
  case Value of
    null -> undefined;
    _ -> list_to_binary(util:to_string(Value))
  end.

can_play({text, _}) -> true;
can_play(_) -> false.
