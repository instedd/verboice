-module(user_default).
-compile([export_all]).

-include("session.hrl").
-include("db.hrl").
-include("uri.hrl").
-include_lib("kernel/include/inet.hrl").
-include_lib("xmerl/include/xmerl.hrl").
-include_lib("inets/include/httpd.hrl").