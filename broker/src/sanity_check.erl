-module(sanity_check).
-export([verify_write_permission_on_sip_file/1, verify_write_permission_on_audio_directory/0, verify_sox/0, verify_write_permission_on_recording_directory/0]).
-include_lib("kernel/include/file.hrl").


verify_write_permission_on_sip_file(FileName) ->
  {ok, BaseConfigPath} = application:get_env(verboice, asterisk_config_dir),
  case filelib:is_dir(BaseConfigPath) of
    false ->
      [{name, list_to_binary(BaseConfigPath)}, {status, error}, {message, list_to_binary("Asterisk config dir does not exist.")}];
    true ->
      FilePath = filename:join(BaseConfigPath, FileName),
      FilePathBinary = list_to_binary(FilePath),
      case file:open(FilePath, [write]) of
        {ok, _} ->
          file:close(FilePath),
          [{name, FilePathBinary}, {status, ok}, {message, list_to_binary("ok")}];
        {error, _} ->
          [{name, FilePathBinary}, {status, error}, {message, list_to_binary("Verboice does not have permissions for write or create the file.")}]
      end
  end.

verify_write_permission_on_audio_directory() ->
  {ok, AudioDirPath} = application:get_env(verboice, asterisk_sounds_dir),
  AudioTestFile = filename:join([AudioDirPath, "verboice",  "test" ++ ".gsm"]),
  verify_write_permission_on_directory(AudioDirPath, "Audio files", AudioTestFile).

verify_write_permission_on_recording_directory() ->
  {ok, RecordDir} = application:get_env(verboice, record_dir),
  RecordTestFile = filename:join([RecordDir, util:to_string("sanity_check"), "results", "test" ++ ".wav"]),
  filelib:ensure_dir(RecordTestFile),
  verify_write_permission_on_directory(RecordDir, "Audio files", RecordTestFile).


verify_write_permission_on_directory(DirPath, DirName, FilePath) ->
  case filelib:is_dir(DirPath) of
   false ->
      [{name, DirPath}, {status, error}, {message, list_to_binary(DirName ++ "directory does not exist.")}];
    true ->
      NameBinary = list_to_binary(DirPath),
      case file:open(FilePath, [write]) of
        {ok, _} ->
          file:close(FilePath),
          file:delete(FilePath),
          [{name, NameBinary}, {status, ok}, {message, list_to_binary("ok")}];
        {error, _} ->
          [{name, NameBinary}, {status, error}, {message, list_to_binary("Verboice does not have permission for creating files in the " ++ DirName ++ " directory.")}]
      end
  end.

verify_sox() ->
   {ok, Data} = file:read_file('priv/sanity_check_test_audio.wav'),
   FileName = "test.gsm",
   sox:convert(Data, "wav", FileName),
   ErrorMessage = [{name, list_to_binary("sox")}, {status, error}, {message, list_to_binary("Sox is not installed or not working.")}],
  case file:read_file_info(FileName) of
    {ok, #file_info{size = Size}} ->
      file:delete(FileName),
      case Size of
        0 ->
          ErrorMessage;
        _ ->
          [{name, list_to_binary("sox")}, {status, ok}, {message, list_to_binary("Sox working ok.")}]
      end;
    {error, _} ->
      ErrorMessage
  end.

