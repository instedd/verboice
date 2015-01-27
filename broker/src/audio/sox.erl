-module(sox).
-export([convert/2, convert/3, convert_with_quality/3]).

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(TIMEOUT, 1000).
-record(state, {port, caller}).

start_link() ->
  gen_server:start_link(?MODULE, {}, []).

convert(Input, OutPath) ->
  {ok, Pid} = start_link(),
  gen_server:call(Pid, {convert, Input, OutPath}).

convert(Input, InType, OutPath) ->
  {ok, Pid} = start_link(),
  gen_server:call(Pid, {convert, Input, InType, OutPath}).

convert_with_quality(Input, Quality, OutPath) ->
  {ok, Pid} = start_link(),
  gen_server:call(Pid, {convert_with_quality, Input, Quality, OutPath}).

%% @private
init({}) ->
  io:format("Sox converter: ~p~n", [self()]),
  {ok, #state{}, ?TIMEOUT}.

%% @private
handle_call({convert, Input, OutPath}, From, State) ->
  Port = do_convert(Input, OutPath),
  {noreply, State#state{port = Port, caller = From}, ?TIMEOUT * 5};

handle_call({convert, Input, InputType, OutPath}, From, State) ->
  Port = do_convert(Input, InputType, OutPath),
  {noreply, State#state{port = Port, caller = From}, ?TIMEOUT * 5};

handle_call({convert_with_quality, Input, Quality, OutPath}, From, State) ->
  Port = do_convert_with_quality(Input, Quality, OutPath),
  {noreply, State#state{port = Port, caller = From}, ?TIMEOUT * 5}.

%% @private
handle_cast(_Msg, State) ->
  {noreply, State, ?TIMEOUT}.

%% @private
handle_info(timeout, State) ->
  {stop, timeout, State};

handle_info({Port, {exit_status, N}}, State = #state{port = Port, caller = Caller}) ->
  Result = case N of 0 -> ok; _ -> error end,
  gen_server:reply(Caller, Result),
  {stop, normal, State};

handle_info(_Info, State) ->
  {noreply, State, ?TIMEOUT}.

%% @private
terminate(_Reason, #state{port = Port}) ->
  case erlang:port_info(Port, connected) of
    {connected, _} -> port_close(Port);
    _ -> ok
  end.

%% @private
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

do_convert(Input, OutPath) when is_list(Input) ->
  open_port({spawn, "sox " ++ Input ++ " -e signed-integer -r 8000 -c1 " ++ OutPath}, [exit_status, stderr_to_stdout]);

do_convert(Input, OutPath) when is_binary(Input) ->
  Port = open_port({spawn, "sox - -e signed-integer -r 8000 -c1 " ++ OutPath}, [binary, exit_status, stderr_to_stdout]),
  port_command(Port, Input),
  Port.

do_convert(Input, InputType, OutPath) when is_list(Input) ->
  Port = open_port({spawn, "sox -t " ++ InputType ++ " " ++ Input ++ " -e signed-integer -r 8000 -c1 " ++ OutPath}, [binary, exit_status, stderr_to_stdout]),
  port_command(Port, Input),
  Port;

do_convert(Input, InputType, OutPath) when is_binary(Input) ->
  Port = open_port({spawn, "sox -t " ++ InputType ++ " - -e signed-integer -r 8000 -c1 " ++ OutPath}, [binary, exit_status, stderr_to_stdout]),
  port_command(Port, Input),
  Port.

do_convert_with_quality(Input, Quality, OutPath) when is_list(Input) ->
  open_port({spawn, "sox " ++ Input ++ " -e signed-integer -r " ++ Quality ++ " -c1 " ++ OutPath}, [exit_status, stderr_to_stdout]);

do_convert_with_quality(Input, Quality, OutPath) when is_binary(Input) ->
  Port = open_port({spawn, "sox - -e signed-integer -r " ++ Quality ++ " -c1 " ++ OutPath}, [binary, exit_status, stderr_to_stdout]),
  port_command(Port, Input),
  Port.
