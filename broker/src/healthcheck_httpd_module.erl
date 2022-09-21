-module(healthcheck_httpd_module).
-export([do/1]).

-include_lib("inets/include/httpd.hrl").
-include("db.hrl").
-compile([{parse_transform, lager_transform}]).

do(#mod{request_uri = "/", method = "GET", entity_body = _, data = _}) ->
  {proceed, [{response, {200, "OK"}}]};

do(#mod{data = Data}) ->
  {proceed, Data}.
