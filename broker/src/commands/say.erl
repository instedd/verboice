-module(say).
-export([run/2]).
-include("session.hrl").

run(Args, Session = #session{pbx = Pbx}) ->
  Text = proplists:get_value(text, Args),
  Resource = resource:prepare_text_resource(list_to_binary(Text), Session),
  Pbx:play(Resource),
  {next, Session}.