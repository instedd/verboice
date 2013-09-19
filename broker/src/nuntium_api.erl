-module(nuntium_api).
-export([send_ao/1]).
-include("uri.hrl").


send_ao(Params) ->
  {ok, Host} = application:get_env(nuntium_host),
  {ok, Account} = application:get_env(nuntium_account),
  {ok, App} = application:get_env(nuntium_app),
  {ok, AppPassword} = application:get_env(nuntium_app_password),

  Uri = uri:parse(Host),
  MsgUri = Uri#uri{path = [$/, Account, $/, App, "/send_ao"], query_string = Params},
  case MsgUri:get([{basic_auth, {[Account, $/, App], AppPassword}}]) of
    {ok, {{_, 200, _}, _, _}} -> ok;
    {ok, {{_, _, Reason}, _, _}} -> {error, Reason};
    {error, Reason} -> {error, io_lib:format("~p", [Reason])}
  end.
