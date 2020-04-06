-module(tts_mac).
-export([synthesize/4, synthesize/3]).

command_args(undefined, TempFile, Text) ->
  ["-o", TempFile, Text];
command_args(Voice, TempFile, Text) ->
  ["-v", Voice, "-o", TempFile, Text].

synthesize(Text, Project, Language, TargetPath) ->
  Voice = Project:voice(Language),
  synthesize(Text, Voice, TargetPath).

synthesize(Text, Voice, TargetPath) ->
  TempFile = TargetPath ++ ".wave",
  CommandArgs = command_args(Voice, TempFile, Text),
  try
    Port = open_port({spawn_executable, "/usr/bin/say"}, [exit_status, CommandArgs]),
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
