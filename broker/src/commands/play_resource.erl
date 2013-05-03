-module(play_resource).
-export([run/2]).
-include("session.hrl").

run(Args, Session = #session{pbx = Pbx}) ->
  Guid = proplists:get_value(resource_guid, Args),
  FileName = resource:prepare(Guid, Session),
  Pbx:play(FileName),
  {next, Session}.

