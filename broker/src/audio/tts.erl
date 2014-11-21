-module(tts).
-export([synthesize/4]).
-compile([{parse_transform, lager_transform}]).

synthesize(Text, Project, Language, TargetPath) ->
  Synth = case Project:tts_engine() of
    "built-in" ->
      poirot:log(info, "Will use built in TTS engine"),
      case os:type() of
        {unix, darwin} -> tts_mac;
        {unix, _} -> tts_festival
      end;
    "ispeech" ->
      poirot:log(info, "Will use iSpeech to synthesize"),
      tts_ispeech
  end,
  Synth:synthesize(Text, Project, Language, TargetPath).