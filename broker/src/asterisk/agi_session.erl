-module(agi_session).
-export([start_link/1, close/1, get_variable/2, ringing/1, answer/1, hangup/1, stream_file/3, wait_for_digit/2, record_file/5, set_callerid/2, dial/2]).

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(TIMEOUT, 10000).

-record(state, {sock, caller, closed}).
-record(response, {code, result, parent, endpos}).

start_link(Sock) ->
  gen_server:start_link(?MODULE, Sock, []).

close(Pid) ->
  gen_server:call(Pid, close).

get_variable(Pid, Variable) ->
  case gen_server:call(Pid, {execute, ["GET VARIABLE " | Variable]}) of
    hangup -> hangup;
    #response{parent = Value} -> {ok, Value}
  end.

ringing(Pid) ->
  gen_server:call(Pid, {execute, "EXEC RINGING"}).

answer(Pid) ->
  case gen_server:call(Pid, {execute, "ANSWER"}) of
    hangup -> hangup;
    #response{result = "0"} -> ok;
    _ -> error
  end.

hangup(Pid) ->
  gen_server:call(Pid, {execute, "HANGUP"}),
  ok.

stream_file(Pid, File, EscapeDigits) ->
  case gen_server:call(Pid, {execute, ["STREAM FILE \"", File, "\" \"", EscapeDigits, "\""]}, infinity) of
    hangup -> hangup;
    #response{result = "-1", endpos = EndPos} -> {hangup, EndPos};
    #response{result = "0", endpos = "0"} -> error;
    #response{result = "0", endpos = EndPos} -> {ok, EndPos};
    #response{result = Digit, endpos = EndPos} -> {digit, list_to_integer(Digit), EndPos}
  end.

wait_for_digit(Pid, Timeout) ->
  case gen_server:call(Pid, {execute, ["WAIT FOR DIGIT ", integer_to_list(Timeout)]}, Timeout + 1000) of
    hangup -> hangup;
    #response{result = "-1"} -> error;
    #response{result = "0"} -> timeout;
    #response{result = Digit} -> {digit, list_to_integer(Digit)}
  end.

record_file(Pid, FileName, Format, StopKeys, Timeout) ->
  case gen_server:call(Pid, {execute, ["RECORD FILE \"", FileName, "\" ", Format, " \"", StopKeys, "\" ", integer_to_list(Timeout), " BEEP"]}, Timeout + 1000) of
    hangup -> hangup;
    #response{result = _, parent = "hangup"} -> hangup;
    #response{result = _, parent = "timeout"} -> timeout;
    #response{result = "-1", parent = _} -> error;
    #response{result = Digit, parent = "dtmf"} -> {digit, list_to_integer(Digit)};
    _ -> error
  end.

set_callerid(Pid, CallerId) ->
  gen_server:call(Pid, {execute, ["SET CALLERID ", CallerId]}).

dial(Pid, ArgList) ->
  Separator = case application:get_env(asterisk_agi_use_pipe_separator) of
    {ok, true} -> "|";
    _ -> ","
  end,
  Args = string:join(ArgList, Separator),
  gen_server:call(Pid, {execute, ["EXEC DIAL ", Args]}, infinity).

%% @private
init(Sock) ->
  case read_params(Sock, []) of
    {ok, Params} ->
      inet:setopts(Sock, [{active, true}]),
      agi_events:notify_new_session(self(), Params),
      {ok, #state{sock = Sock}, ?TIMEOUT};
    Error -> {stop, Error}
  end.

%% @private
handle_call({execute, _}, _From, State = #state{closed = true}) ->
  {reply, hangup, State};

handle_call({execute, Cmd}, From, State = #state{sock = Sock}) ->
  gen_tcp:send(Sock, [Cmd | "\n"]),
  {noreply, State#state{caller = From}};

handle_call(close, _From, State) ->
  {stop, normal, ok, State#state{closed = true}};

handle_call(_Request, _From, State) ->
  {reply, {error, unknown_call}, State}.

%% @private
handle_cast(_Msg, State) ->
  {noreply, State}.

%% @private
handle_info({tcp, _, <<"HANGUP", _/binary>>}, State = #state{sock = Sock}) ->
  gen_tcp:close(Sock),
  {noreply, State#state{closed = true}};

handle_info({tcp, _, Line}, State = #state{caller = From}) ->
  Response = parse_response(Line),
  gen_server:reply(From, Response),
  {noreply, State#state{caller = undefined}, ?TIMEOUT};

handle_info(timeout, State) ->
  {stop, timeout, State#state{closed = true}};

handle_info({tcp_closed, _}, State) ->
  {stop, closed, State#state{closed = true}};

handle_info(_Info, State) ->
  {noreply, State}.

%% @private
terminate(_Reason, #state{sock = Sock}) ->
  gen_tcp:close(Sock),
  ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

read_params(Sock, Params) ->
  case gen_tcp:recv(Sock, 0) of
    {ok, Bin} ->
      case util:strip_nl(Bin) of
        <<>> -> {ok, Params};
        <<"agi_", Line/binary>> ->
          read_params(Sock, [parse_param(Line) | Params]);
        _ -> read_params(Sock, Params)
      end;
    Error -> Error
  end.

parse_param(Line) ->
  [ParamName, ParamValue] = binary:split(Line, [<<": ">>, <<":">>]),
  {util:binary_to_lower_atom(ParamName), ParamValue}.

parse_response(Line) ->
  {match, Match} = re:run(Line,
    "^(\\d+)\\s+result=(-?[^\\s]*)(?:\\s+\\((.*)\\))?(?:\\s+endpos=(-?\\d+))?",
    [{capture, all_but_first, list}]),
  parse_response(1, Match, #response{}).

parse_response(_, [], Response) -> Response;
parse_response(1, [Code|R], Response) ->
  parse_response(2, R, Response#response{code = Code});
parse_response(2, [Result|R], Response) ->
  parse_response(3, R, Response#response{result = Result});
parse_response(3, [Parent|R], Response) ->
  parse_response(4, R, Response#response{parent = Parent});
parse_response(4, [EndPos|_], Response) ->
  Response#response{endpos = EndPos}.
