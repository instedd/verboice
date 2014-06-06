-module(twilio_channel_srv).
-export([start_link/0, reload_channels/0, find_channel/2, get_channel_status/1]).

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-record(state, {registry = dict:new(), channel_status = dict:new(), status_job_state = idle}).
-include("db.hrl").

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, {}, []).

reload_channels() ->
  gen_server:cast(?SERVER, reload_channels).

find_channel(AccountSid, Number) ->
  gen_server:call(?SERVER, {find_channel, AccountSid, Number}).

get_channel_status(ChannelIds) ->
  gen_server:call(?SERVER, {get_channel_status, ChannelIds}).

%% @private
init({}) ->
  reload_channels(),
  timer:send_interval(timer:minutes(2), check_status),
  {ok, #state{}}.

%% @private
handle_call({find_channel, AccountSid, Number}, _From, State = #state{registry = Registry}) ->
  case dict:find({AccountSid, util:normalize_phone_number(Number)}, Registry) of
    {ok, ChannelId} -> {reply, ChannelId, State};
    error -> {reply, undefined, State}
  end;

handle_call({get_channel_status, ChannelIds}, _From, State = #state{channel_status = Status}) ->
  Result = lists:foldl(fun(ChannelId, S) ->
    case dict:find(ChannelId, Status) of
      {ok, ChannelStatus} -> [ChannelStatus | S];
      error -> S
    end
  end, [], ChannelIds),
  {reply, Result, State};

handle_call(_Request, _From, State) ->
  {reply, {error, unknown_call}, State}.

%% @private
handle_cast(reload_channels, State) ->
  Channels = channel:find_all_twilio(),
  NewRegistry = lists:foldl(fun(Channel, Registry) ->
    AccountSid = channel:account_sid(Channel),
    Number = util:normalize_phone_number(channel:number(Channel)),
    dict:store({AccountSid, Number}, Channel, Registry)
  end, dict:new(), Channels),

  {noreply, State#state{registry = NewRegistry}};

handle_cast({set_channel_status, NewStatus}, State = #state{channel_status = PrevStatus}) ->
  channel:log_broken_channels(PrevStatus, NewStatus),
  {noreply, State#state{channel_status = NewStatus, status_job_state = idle}};

handle_cast(_Msg, State) ->
  {noreply, State}.

%% @private
handle_info(check_status, State = #state{registry = Registry, status_job_state = idle}) ->
  spawn(fun() ->
    try
      NewStatus = dict:fold(fun(_, Channel = #channel{id = Id, config = _Config}, Status) ->
        ChannelState = case twilio_api:incoming_phone_numbers(Channel) of
          {ok, Response} ->
            case proplists:get_value(<<"incoming_phone_numbers">>, Response) of
              [_|_] -> {Id, true, []};
              _ -> {Id, false, [<<"Incoming number not found">>]}
            end;
          {error, Reason} when is_list(Reason) -> {Id, false, [list_to_binary(Reason)]};
          _ -> {Id, false, []}
        end,
        dict:store(Id, ChannelState, Status)
      end, dict:new(), Registry),
      gen_server:cast(?SERVER, {set_channel_status, NewStatus})
    catch
      _:_ -> gen_server:cast(?SERVER, {set_channel_status, dict:new()})
    end
  end),
  {noreply, State#state{status_job_state = working}};

handle_info(_Info, State) ->
  {noreply, State}.

%% @private
terminate(_Reason, _State) ->
  ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
