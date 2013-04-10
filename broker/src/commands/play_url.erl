-module(play_url).
-export([run/2]).
-include("session.hrl").

run(Args, #session{pbx = Pbx}) ->
  Url = proplists:get_value(url, Args),
  FileName = resource:prepare_url_resource(Url, Pbx),
  Pbx:play(FileName),
  next.
