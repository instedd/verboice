-module(verboice_config).
-on_load(load_config/0).
-define(METHOD_TPL(M), -export([M/0])).
-include("config_methods.hrl").

load_config() ->
  load(asterisk_config_dir, string),
  load(asterisk_sounds_dir, string),
  load(asterisk_record_dir, string),
  load(broker_host, string),
  load(broker_port, int),
  load(broker_bind, atom),
  load(ami_host, string),
  load(ami_port, int),
  load(asterisk_agi_use_pipe_separator, bool),
  load(base_url, string),
  load(record_dir, string),
  load(seconds_between_calls, int),
  load(seconds_for_call_back, int),
  load(db_name, string),
  load(db_user, string),
  load(db_pass, string),
  load(db_host, string),
  load(crypt_secret, string),
  load(nuntium_host, string),
  load(nuntium_account, string),
  load(nuntium_app, string),
  load(nuntium_app_password, string),
  load(guisso_client_id, string),
  load(guisso_client_secret, string),
  load(guisso_url, string),
  load(broker_httpd_base_url,string),
  load(twilio_callback_url, string),
  load(twilio_base_url, string),
  load(hub_enabled, bool),
  load(hub_url, string),
  ok.

-define(METHOD_TPL(Name), Name() -> application:get_env(verboice, Name, undefined)).
-include("config_methods.hrl").

load(Name, Type) ->
  EnvName = string:to_upper(erlang:atom_to_list(Name)),
  case os:getenv(EnvName) of
    false -> not_found;
    Value ->
      case parse_value(Value, Type) of
        invalid ->
          io:format("ERROR: Invalid value \"~s\" for parameter ~s~n", [Value, EnvName]);
        TypedValue ->
          application:set_env(verboice, Name, TypedValue)
      end
  end.

parse_value(Value, int) ->
  case string:to_integer(Value) of
    {IntValue, []} ->
      IntValue;
    _ ->
      invalid
  end;

parse_value(Value, bool) ->
  case list_to_atom(Value) of
    true -> true;
    false -> false;
    _ -> invalid
  end;

parse_value(Value, atom) -> list_to_atom(Value);

parse_value(Value, string) -> Value.
