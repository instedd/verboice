-module(test_app).
-export([start/0, stop/1, run_test_in_transaction/1, wait_process/1]).

start() ->
  {ok, Cwd} = file:get_cwd(),
  file:set_cwd(filename:join(Cwd, "..")),
  application:load({application, verboice, [
    {description, ""},
    {vsn, "1"},
    {registered, []},
    {applications, [kernel, stdlib, inets, ssl, poirot]},
    {mod, { verboice_app, []}},
    {env, [
      {db_name, "verboice_test"},
      {db_user, "root"},
      {db_host, "db"},
      {db_pass, ""},
      {asterisk_config_dir, "/usr/local/asterisk/etc/asterisk"},
      {asterisk_sounds_dir, "/usr/local/asterisk/var/lib/asterisk/sounds"},
      {asterisk_agi_use_pipe_separator, false},
      {broker_port, 19000},
      {base_url, "http://localhost:3000"},
      {hub_enabled, false},
      {seconds_between_calls, 2},
      {minutes_between_active_calls_gc_runs, 10}
    ]}
  ]}),

  {ok, _} = application:ensure_all_started(verboice),
  file:set_cwd(Cwd).

stop(_) ->
  application:stop(verboice).

run_test_in_transaction(Fun) ->
  db:update("BEGIN"),
  try Fun()
  after
    db:update("ROLLBACK")
  end.

wait_process(Pid) ->
  MonitorRef = monitor(process, Pid),
  receive
    {'DOWN', MonitorRef, process, Pid, Info} -> Info
  after 1000 -> throw(timeout)
  end.
