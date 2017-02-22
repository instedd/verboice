-module(tts_festival).
-export([synthesize/4, synthesize/3]).

synthesize(Text, Project, Language, TargetPath) ->
  Voice = Project:voice(Language),
  synthesize(Text, Voice, TargetPath).

synthesize(Text, Voice, TargetPath) when is_binary(Text) ->
  ConvertedText = case unicode:characters_to_list(Text, unicode) of
                    L when is_list(L)   -> L;
                    {error, _, _}       -> binary_to_list(Text);
                    {incomplete, L1, _} -> L1
                  end,
  synthesize(ConvertedText, Voice, TargetPath);

synthesize(Text, Voice, TargetPath) ->
  TempFile = TargetPath ++ ".wav",
  InputFile = TargetPath ++ ".txt",
  file:write_file(InputFile, Text, [{encoding, latin1}]),
  try
    Exec = os:find_executable("text2wave"),
    Port = open_port({spawn_executable, Exec}, [exit_status,
      {args, ["-eval", "(voice.select '" ++ Voice ++ ")", "-o", TempFile, InputFile]}]),
    receive
      {Port, {exit_status, N}} ->
        case N of
          0 -> sox:convert(TempFile, TargetPath), ok;
          _ -> error
        end
      after 5000 ->
        timeout
    end
  after
    file:delete(TempFile),
    file:delete(InputFile)
  end.

