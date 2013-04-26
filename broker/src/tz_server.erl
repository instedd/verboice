-module(tz_server).
-export([start_link/0, get_timezone_offset/1]).

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, {}, []).

get_timezone_offset(TimeZone) when is_binary(TimeZone) ->
  gen_server:call(?SERVER, {get_offset, TimeZone});

get_timezone_offset(TimeZone) when is_list(TimeZone) ->
  get_timezone_offset(list_to_binary(TimeZone)).

%% @private
init({}) ->
  Port = open_port({spawn, "ruby priv/tz_server.rb"}, [stream, binary, exit_status]),
  {ok, Port}.

%% @private
handle_call({get_offset, TimeZone}, _From, Port) ->
  port_command(Port, <<TimeZone/binary, "\n">>),
  receive
    {Port, {data, Result}} ->
      ResultList = binary_to_list(util:strip_nl(Result)),
      Response = try list_to_integer(ResultList) of
        N -> N
      catch
        error:badarg ->
          {error, ResultList}
      end,
      {reply, Response, Port}

    after 1000 ->
      {stop, timeout, Port}
  end;

handle_call(_Request, _From, Port) ->
  {reply, {error, unknown_call}, Port}.

%% @private
handle_cast(_Msg, Port) ->
  {noreply, Port}.

%% @private
handle_info({Port, {exit_status, _}}, Port) ->
  {stop, port_down, Port};

handle_info(_Info, Port) ->
  {noreply, Port}.

%% @private
terminate(_Reason, Port) ->
  case erlang:port_info(Port, connected) of
    {connected, _} -> port_close(Port);
    _ -> ok
  end.

%% @private
code_change(_OldVsn, Port, _Extra) ->
  {ok, Port}.