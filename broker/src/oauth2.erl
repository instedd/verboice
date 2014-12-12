-module(oauth2).
-export([client_credentials_token/2, authorization_header/3]).

-include("uri.hrl").
-include("oauth2.hrl").
-record(access_token, {access_token, token_type, expires_in, mac_key, mac_algorithm}).

client_credentials_token(Config, Scope) ->
  BaseUrl = Config#oauth2_config.base_url,
  TokenPath = Config#oauth2_config.token_path,
  TokenUrl = (uri:parse(BaseUrl))#uri{path = TokenPath},

  ClientId = Config#oauth2_config.client_id,
  ClientSecret = Config#oauth2_config.client_secret,

  Query = [{grant_type, client_credentials}, {scope, Scope}],

  {ok, {_, _, AccessTokenBody}} = TokenUrl:post_form(Query, [{basic_auth, {ClientId, ClientSecret}}]),

  {ok, {AccessToken}} = json:decode(AccessTokenBody),

  parse_access_token(AccessToken, #access_token{}).

parse_access_token([], Token) -> Token;
parse_access_token([{Key, Value} | R], Token) ->
  NewToken = case Key of
    <<"access_token">> -> Token#access_token{access_token = Value};
    <<"token_type">> -> Token#access_token{token_type = Value};
    <<"expires_in">> -> Token#access_token{expires_in = Value};
    <<"mac_key">> -> Token#access_token{mac_key = Value};
    <<"mac_algorithm">> -> Token#access_token{mac_algorithm = Value};
    _ -> Token
  end,
  parse_access_token(R, NewToken).

authorization_header(Method, Uri, AccessToken = #access_token{token_type = <<"mac">>}) ->
  Now = calendar:universal_time(),
  UnixEpoch = {{1970,1,1},{0,0,0}},
  Ts = calendar:datetime_to_gregorian_seconds(Now) - calendar:datetime_to_gregorian_seconds(UnixEpoch),
  Nonce = [integer_to_list(Ts), $:, base64:encode(crypto:strong_rand_bytes(16))],
  Path = Uri:full_path(),
  Host = Uri#uri.host,
  Port = Uri#uri.port,
  Ext = "",
  MacAlgorithm = AccessToken#access_token.mac_algorithm,
  MacKey = AccessToken#access_token.mac_key,

  Mac = mac_signature(Ts, Nonce, Method, Path, Host, Port, Ext, MacAlgorithm, MacKey),

  ["Mac id=\"", AccessToken#access_token.access_token, "\", nonce=\"", Nonce, "\", ts=\"", integer_to_list(Ts), "\", mac=\"", Mac, "\""].

mac_signature(Ts, Nonce, Method, Path, Host, Port, Ext, MacAlgorithm, MacKey) ->
  NormalizedRequestString = [integer_to_list(Ts), "\n", Nonce, "\n", Method, "\n", Path, "\n", Host, "\n", integer_to_list(Port), "\n", Ext, "\n"],

  Digest = case MacAlgorithm of
    <<"hmac-sha-1">> -> sha;
    <<"hmac-sha-256">> -> sha256
  end,

  Mac = crypto:hmac(Digest, MacKey, NormalizedRequestString),
  base64:encode(Mac).
