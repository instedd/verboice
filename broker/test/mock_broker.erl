-module(mock_broker).
-behaviour(broker).
-export([start/0, wait_dispatch/1, handle_cast/2, init/0, create_channel/1, destroy_channel/1, dispatch/1]).
-include("session.hrl").
-include("db.hrl").

start() ->
  meck:new(channel, [passthrough]),
  meck:expect(channel, broker, 1, mock_broker),
  broker:start_link(mock_broker),
  gen_server:cast(mock_broker, {set_test_pid, self()}).

wait_dispatch(QueuedCallId) ->
  Session = receive
    S = #session{queued_call = #queued_call{id = QueuedCallId}} -> S
  after 1000 ->
    throw(timeout)
  end,
  session:find(Session#session.session_id).

init() -> undefined.

create_channel(_) -> throw(not_implemented).
destroy_channel(_) -> throw(not_implemented).

handle_cast({set_test_pid, TestPid}, _) ->
  TestPid;

handle_cast({dispatch_session, Session}, TestPid) ->
  TestPid ! Session,
  TestPid.

dispatch(Session) ->
  gen_server:cast(mock_broker, {dispatch_session, Session}).
