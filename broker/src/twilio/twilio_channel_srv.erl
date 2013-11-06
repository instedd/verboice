-module(twilio_channel_srv).
-export([start_link/0, reload_channels/0, find_channel/2]).

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-record(state, {registry = dict:new()}).
-include("db.hrl").

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, {}, []).

reload_channels() ->
  gen_server:cast(?SERVER, reload_channels).

find_channel(AccountSid, Number) ->
  gen_server:call(?SERVER, {find_channel, AccountSid, Number}).

%% @private
init({}) ->
  reload_channels(),
  {ok, #state{}}.

%% @private
handle_call({find_channel, AccountSid, Number}, _From, State = #state{registry = Registry}) ->
  case dict:find({AccountSid, util:normalize_phone_number(Number)}, Registry) of
    {ok, ChannelId} -> {reply, ChannelId, State};
    error -> {reply, undefined, State}
  end;

handle_call(_Request, _From, State) ->
  {reply, {error, unknown_call}, State}.

%% @private
handle_cast(reload_channels, State) ->
  Channels = channel:find_all_twilio(),
  NewRegistry = lists:foldl(fun(#channel{id = Id, config = Config}, Registry) ->
    AccountSid = proplists:get_value("account_sid", Config),
    Number = util:normalize_phone_number(proplists:get_value("number", Config)),
    dict:store({AccountSid, Number}, Id, Registry)
  end, dict:new(), Channels),

  {noreply, State#state{registry = NewRegistry}};

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
