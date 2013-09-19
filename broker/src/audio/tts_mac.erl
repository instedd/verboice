-module(tts_mac).
-export([synthesize/4, synthesize/3]).

synthesize(Text, Project, Language, TargetPath) ->
  Voice = Project:voice(Language),
  synthesize(Text, Voice, TargetPath).

synthesize(Text, Voice, TargetPath) ->
  TempFile = TargetPath ++ ".wave",
  try
    Port = open_port({spawn_executable, "/usr/bin/say"}, [exit_status, {args, ["-v", Voice, "-o", TempFile, Text]}]),
    receive
      {Port, {exit_status, N}} ->
        case N of
          0 -> sox:convert(TempFile, TargetPath);
          _ -> error
        end
      after 5000 ->
        timeout
    end
  after
    file:delete(TempFile)
  end.

