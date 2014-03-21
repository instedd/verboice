-module(hangup_and_callback).
-export([run/2]).
-include("db.hrl").
-include("session.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

run(Args, Session) ->
  Prefix = proplists:get_value(dial_prefix, Args),
  When = proplists:get_value('when', Args),

  case When of
    "later" ->
      Delay = proplists:get_value(delay, Args),
      Seconds = util:parse_short_time(Delay),
      CallDate = util:time_from_now(Seconds),
      QueuedCall = queued_call:create(#queued_call{
        channel_id = Session#session.channel#channel.id,
        call_log_id = (Session#session.call_log):id(),
        address = Session#session.address,
        % TODO: callback_url
        flow = [],
        status_callback_url = Session#session.status_callback_url,
        % TODO: schedule (maybe?)
        not_before = {datetime, CallDate},
        % TODO: retries
        project_id = Session#session.project#project.id,
        call_flow_id = Session#session.call_flow#call_flow.id,
        % TODO: time_zone
        session_id = Session#session.session_id,
        variables = [],
        callback_params = []
      }),
      scheduler:enqueue(QueuedCall),
      {hibernate, Session};
    _ ->
      NotBefore = util:time_from_now(15),
      scheduler:enqueue(#queued_call{
        not_before = {datetime, NotBefore},
        session_id = Session#session.session_id,
        channel_id = Session#session.channel#channel.id,
        address = dial_address(Session#session.address, Prefix)
      }),
      {suspend, Session}
  end.

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
