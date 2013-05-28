-module(asterisk_broker).
-export([start_link/0, init/0, notify_ready/0, dispatch/1]).

-behaviour(broker).

-include("session.hrl").
-include("db.hrl").

start_link() ->
  broker:start_link(?MODULE).

init() ->
  ami_events:add_handler(asterisk_event_handler, []),
  ami_client:connect().

notify_ready() ->
  broker:notify_ready(?MODULE).

dial_address(#channel{type = <<"Channels::Custom">>, config = Config}, Address) ->
  DialString = proplists:get_value(<<"dial_string">>, Config),
  util:interpolate(DialString, fun(Key) ->
    case Key of
      <<"number">> -> Address;
      _ -> <<>>
    end
  end);

dial_address(#channel{id = Id}, Address) ->
  ["SIP/verboice_", integer_to_list(Id), "-outbound/", Address].

dispatch(#session{session_id = SessionId, channel = Channel, address = Address}) ->
  DialAddress = dial_address(Channel, Address),
  {ok, BrokerPort} = application:get_env(broker_port),
  ami_client:originate([
    {channel, DialAddress},
    {application, "AGI"},
    {data, ["agi://localhost:", integer_to_list(BrokerPort), ",", SessionId]},
    {async, true},
    {actionid, SessionId}
  ]).
