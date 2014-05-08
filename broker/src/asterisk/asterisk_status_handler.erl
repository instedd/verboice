-module(asterisk_status_handler).
-behaviour(gen_event).
-export([start/1, init/1, handle_event/2, handle_call/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {registry, status}).

start(RegistryIndex) ->
  ami_events:add_handler(?MODULE, RegistryIndex).

%% @private
init(RegistryIndex) ->
  case ami_client:sip_show_registry() of
    {error, Reason} -> {error, Reason};
    _ -> {ok, #state{registry = RegistryIndex, status = dict:new()}}
  end.

handle_event({registryentry, Packet}, State = #state{registry = Registry, status = Status}) ->
  Event = ami_client:decode_packet(Packet),
  Username = binary_to_list(proplists:get_value(username, Event)),
  Host = binary_to_list(proplists:get_value(host, Event)),
  EntryState = proplists:get_value(state, Event),

  NewStatus = case dict:find({Username, Host}, Registry) of
    error -> Status;
    {ok, ChannelId} ->
      Status2 = {_, _, Messages} = case dict:find(ChannelId, Status) of
        error -> {ChannelId, true, []};
        {ok, S} -> S
      end,
      ChannelStatus = case EntryState of
        <<"Registered">> -> Status2;
        _ ->
          Message = iolist_to_binary(["Host ", Host, ", status: ", EntryState]),
          {ChannelId, false, [Message | Messages]}
      end,
      dict:store(ChannelId, ChannelStatus, Status)
  end,
  {ok, State#state{status = NewStatus}};

handle_event({registrationscomplete, _}, #state{status = Status}) ->
  asterisk_channel_srv:set_channel_status(Status),
  remove_handler;

handle_event(_Event, State) ->
  {ok, State}.

%% @private
handle_call(_Request, _State) ->
  {remove_handler, {error, unknown_call}}.

%% @private
handle_info(_Info, State) ->
  {ok, State}.

%% @private
terminate(_Reason, _State) ->
  ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
