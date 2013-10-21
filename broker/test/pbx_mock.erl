-module(pbx_mock).
% -behaviour(pbx).
-export([new/1, pid/1, answer/1, hangup/1, terminate/1, can_play/2, play/2]). %, capture/6, record/4, terminate/1, sound_path_for/2, dial/4]).
-export([validate/1]).

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

new(Sequence) ->
  {ok, Pid} = gen_server:start_link({local, ?SERVER}, ?MODULE, Sequence, []),
  {pbx_mock, Pid}.

pid({pbx_mock, Pid}) -> Pid.

answer({pbx_mock, Pid}) -> invoke(Pid, answer, []).
hangup({pbx_mock, Pid}) -> invoke(Pid, hangup, []).
can_play(ResourceKind, {pbx_mock, Pid}) -> invoke(Pid, can_play, [ResourceKind]).
play(Resource, {pbx_mock, Pid}) -> invoke(Pid, play, [Resource]).

terminate({pbx_mock, Pid}) ->
  case gen_server:call(Pid, terminate) of
    ok -> ok;
    error -> erlang:error(unexpected_terminate)
  end.

validate({pbx_mock, Pid}) ->
  case gen_server:call(Pid, validate) of
    [] -> ok;
    Sequence -> erlang:error({missing_calls, Sequence})
  end.

invoke(Pid, Method, Args) ->
  case gen_server:call(Pid, {invoke, Method, Args}) of
    {ok, Fun} when is_function(Fun) -> Fun();
    {ok, Result} -> Result;
    {error, Error} -> erlang:error(Error)
  end.

%% @private
init(Sequence) ->
  {ok, Sequence}.

%% @private
handle_call({invoke, Method, Args}, _From, [{Method, Args, Response} | Sequence]) ->
  {reply, {ok, Response}, Sequence};

handle_call({invoke, Method, Args}, _From, Sequence) ->
  {reply, {error, {unexpected_call, Method, Args}}, Sequence};

handle_call(terminate, _From, Sequence) ->
  Response = case Sequence of
    [] -> ok;
    _  -> error
  end,
  {reply, Response, Sequence};

handle_call(validate, _From, Sequence) ->
  {stop, normal, Sequence, Sequence};

handle_call(_Request, _From, State) ->
  {reply, {error, unknown_call}, State}.

%% @private
handle_cast(_Msg, State) ->
  {noreply, State}.

%% @private
handle_info(_Info, State) ->
  {noreply, State}.

%% @private
terminate(_Reason, _State) ->
  ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

