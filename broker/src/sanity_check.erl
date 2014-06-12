-module(sanity_check).

-export([verify_write_permission_on_sip_file/1, verify_write_permission_on_audio_directory/0]).


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
  case filelib:is_dir(AudioDirPath) of
   false ->
      [{name, AudioDirPath}, {status, error}, {message, list_to_binary("Audio file directory does not exist.")}];
    true ->
      FilePath = filename:join([AudioDirPath, "verboice",  "test" ++ ".gsm"]),
      NameBinary = list_to_binary(AudioDirPath),
      case file:open(FilePath, [write]) of
        {ok, _} ->
          file:close(FilePath),
          file:delete(FilePath),
          [{name, NameBinary}, {status, ok}, {message, list_to_binary("ok")}];
        {error, _} ->
          [{name, NameBinary}, {status, error}, {message, list_to_binary("Verboice does not have permission for creating files in the audio directory.")}]
      end
  end.

