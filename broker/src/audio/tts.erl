-module(tts).
-export([synthesize/4]).

synthesize(Text, Project, Language, TargetPath) ->
  Synth = case Project:tts_engine() of
    "built-in" ->
      case os:type() of
        {unix, darwin} -> tts_mac;
        {unix, _} -> tts_festival
      end;
    "ispeech" -> tts_ispeech
  end,
  Synth:synthesize(Text, Project, Language, TargetPath).