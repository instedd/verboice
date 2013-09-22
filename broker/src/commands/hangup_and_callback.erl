-module(hangup_and_callback).
-export([run/2]).
-include("db.hrl").
-include("session.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

run(Args, Session) ->
  Prefix = proplists:get_value(dial_prefix, Args),

  NotBefore = calendar:gregorian_seconds_to_datetime(calendar:datetime_to_gregorian_seconds(calendar:universal_time()) + 15),
  scheduler:enqueue(#queued_call{
    not_before = {datetime, NotBefore},
    session_id = Session#session.session_id,
    channel_id = Session#session.channel#channel.id,
    address = dial_address(Session#session.address, Prefix)
  }),

  {suspend, Session}.

dial_address(Address, undefined) -> Address;
dial_address(Address, []) -> Address;
dial_address(Address, Prefix) ->
  PrefixBin = list_to_binary(Prefix),
  PrefixSize = byte_size(PrefixBin),
  case Address of
    <<PrefixBin:PrefixSize/binary, _/binary>> -> Address;
    _ -> <<PrefixBin/binary, Address/binary>>
  end.

-ifdef(TEST).
dial_address_test() ->
  ?assertEqual(<<"123">>, dial_address(<<"123">>, undefined)),
  ?assertEqual(<<"123">>, dial_address(<<"123">>, [])),
  ?assertEqual(<<"0123">>, dial_address(<<"123">>, "0")),
  ?assertEqual(<<"123">>, dial_address(<<"123">>, "12")).
-endif.
