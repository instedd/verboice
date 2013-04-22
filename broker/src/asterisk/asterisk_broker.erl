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

dispatch(Session = #session{session_id = SessionId}) ->
  ChannelId = integer_to_list(Session#session.channel#channel.id),
  Address = "SIP/verboice_" ++ ChannelId ++ "-outbound/" ++ binary_to_list(Session#session.address),
  ami_client:originate([
    {channel, Address},
    {application, "AGI"},
    {data, "agi://localhost:6666," ++ SessionId},
    {async, true},
    {actionid, SessionId}
  ]).