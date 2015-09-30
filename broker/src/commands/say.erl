-module(say).
-export([run/2]).
-include("session.hrl").

run(Args, Session = #session{pbx = Pbx}) ->
  Text = proplists:get_value(text, Args),
  Language = proplists:get_value(language, Args),
  Resource = resource:prepare_text_resource(list_to_binary(Text), Language, Session),
  Pbx:play(Resource),
  {next, Session}.
