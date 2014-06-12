-module(sanity_check).

-export([verify_write_permission_to_sip_file/1]).

verify_write_permission_to_sip_file(FileName) ->
  {ok, BaseConfigPath} = application:get_env(verboice, asterisk_config_dir),
  case filelib:is_file(BaseConfigPath) of
    false ->
      [{name,BaseConfigPath}, {status, error}, {message, "Asterisk config dir does not exist."}];
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
