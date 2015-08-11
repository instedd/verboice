-module(sanity_check).
-export([list/0, run/1, check_all/0]).
-include_lib("kernel/include/file.hrl").

-define(CHECKS, [
  {<<"Write permissions on 'sip_verboice_channels.conf'">>, fun verify_write_permission_on_sip_file/1, ["sip_verboice_channels.conf"]},
  {<<"Write permissions on 'sip_verboice_registrations.conf'">>, fun verify_write_permission_on_sip_file/1, ["sip_verboice_registrations.conf"]},
  {<<"Write permissions on audio directory">>, fun verify_write_permission_on_audio_directory/0, []},
  {<<"Write permissions on recordings directory">>, fun verify_write_permission_on_recording_directory/0, []},
  {<<"Verify SOX is installed">>, fun verify_sox/0, []}
]).

list() ->
  lists:map(fun({Name, _, _}) -> Name end, ?CHECKS).

run(N) ->
  {_, Fun, Args} = lists:nth(N, ?CHECKS),
  erlang:apply(Fun, Args).

run_all() ->
  lists:map(fun({Name, Fun, Args}) -> [{name, Name} | erlang:apply(Fun, Args)] end, ?CHECKS).

check_all() ->
  Statuses = run_all(),
  case lists:any(fun(Result) -> proplists:get_value(status, Result) =:= error end, Statuses) of
    true ->
      {error, lists:map(fun(Result) ->
        { binary_to_list(proplists:get_value(name, Result)), binary_to_list(proplists:get_value(message, Result)) }
      end, Statuses)};
    false ->
      ok
  end.

verify_write_permission_on_sip_file(FileName) ->
  {ok, BaseConfigPath} = application:get_env(verboice, asterisk_config_dir),
  case filelib:is_dir(BaseConfigPath) of
    false ->
      [{status, error}, {message, list_to_binary("Asterisk config dir does not exist.")}];
    true ->
      FilePath = filename:join(BaseConfigPath, FileName),
      case file:open(FilePath, [write]) of
        {ok, _} ->
          file:close(FilePath),
          [{status, ok}, {message, list_to_binary("ok")}];
        {error, _} ->
          [{status, error}, {message, list_to_binary("Verboice does not have permissions for write or create the file.")}]
      end
  end.

verify_write_permission_on_audio_directory() ->
  {ok, AudioDirPath} = application:get_env(verboice, asterisk_sounds_dir),
  AudioTestDir = filename:join([AudioDirPath, "verboice"]),
  AudioTestFile = filename:join([AudioTestDir,  "test" ++ ".gsm"]),
  verify_write_permission_on_directory(AudioTestDir, "audio files", AudioTestFile).

verify_write_permission_on_recording_directory() ->
  {ok, RecordDir} = application:get_env(verboice, record_dir),
  RecordTestFile = filename:join([RecordDir, util:to_string("sanity_check"), "results", "test" ++ ".wav"]),
  filelib:ensure_dir(RecordTestFile),
  verify_write_permission_on_directory(RecordDir, "recording files", RecordTestFile).

verify_write_permission_on_directory(DirPath, DirName, FilePath) ->
  case filelib:is_dir(DirPath) of
   false ->
      [{status, error}, {message, list_to_binary("The " ++ DirName ++ " directory " ++ DirPath ++ " does not exist.")}];
    true ->
      case file:open(FilePath, [write]) of
        {ok, _} ->
          file:close(FilePath),
          file:delete(FilePath),
          [{status, ok}, {message, list_to_binary("ok")}];
        {error, _} ->
          [{status, error}, {message, list_to_binary("Verboice does not have permission for creating files in the " ++ DirName ++ " directory " ++ DirPath)}]
      end
  end.

verify_sox() ->
   {ok, Data} = file:read_file('priv/sanity_check_test_audio.wav'),
   FileName = "test.gsm",
   sox:convert(Data, "wav", FileName),
   ErrorMessage = [{status, error}, {message, list_to_binary("Sox is not installed or not working.")}],
  case file:read_file_info(FileName) of
    {ok, #file_info{size = Size}} ->
      file:delete(FileName),
      case Size of
        0 ->
          ErrorMessage;
        _ ->
          [{status, ok}, {message, list_to_binary("Sox working ok.")}]
      end;
    {error, _} ->
      ErrorMessage
  end.
