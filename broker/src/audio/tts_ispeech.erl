-module(tts_ispeech).
-export([synthesize/4, ispeech_synth/4]).

synthesize(Text, Project, Language, TargetPath) ->
  Voice = Project:voice(Language),
  ApiKey = Project:ispeech_api_key(),
  ispeech_synth(Text, Voice, ApiKey, TargetPath).

ispeech_synth(Text, Voice, ApiKey, TargetPath) ->

  Url = "http://api.ispeech.org/api/rest?apikey=" ++ ApiKey
    ++ "&action=convert&voice=" ++ Voice
    ++ "&format=mp3&text=" ++ httpd_util:encode_hex(util:to_string(Text)),

  {ok, {_, _, Body}} = httpc:request(Url),

  TempFile = TargetPath ++ ".mp3",
  file:write_file(TempFile, Body),
  try
    sox:convert(TempFile, TargetPath)
  after
    file:delete(TempFile)
  end.
