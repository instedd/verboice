-module(guisso).
-export([get_trusted_token/2]).

-include("oauth2.hrl").

get_trusted_token(App, Email) ->
  Scope = ["app=", App, " user=", Email],
  oauth2:client_credentials_token(oauth2_config(), Scope).

oauth2_config() ->
  {ok, Url} = application:get_env(verboice, guisso_url),
  {ok, ClientId} = application:get_env(verboice, guisso_client_id),
  {ok, ClientSecret} = application:get_env(verboice, guisso_client_secret),

  #oauth2_config{
    base_url = Url,
    client_id = ClientId,
    client_secret = ClientSecret
  }.
