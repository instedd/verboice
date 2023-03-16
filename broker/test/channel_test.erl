-module(channel_test).
-include_lib("eunit/include/eunit.hrl").
-include_lib("db.hrl").

base_url_test() ->
  ?assertEqual("http://localhost:3000", channel:base_url(#channel{ type = <<"Channels::Twilio">>, config = [{"base_url", "http://localhost:3000"}] })),
  ?assertEqual(verboice_config:twilio_base_url(), channel:base_url(#channel{ type = <<"Channels::Twilio">>, config = [{}] })),
  ?assertEqual(verboice_config:twilio_base_url(), channel:base_url(#channel{ type = <<"Channels::Twilio">>, config = [{"base_url", ""}] })),
  ?assertEqual(verboice_config:twilio_base_url(), channel:base_url(#channel{ type = <<"Channels::Twilio">>, config = [{"base_url", nil}] })),
  ?assertEqual(verboice_config:twilio_base_url(), channel:base_url(#channel{ type = <<"Channels::Twilio">>, config = [{"base_url", undefined}] })),
  ?assertEqual(nil, channel:base_url(#channel{ type = <<"Channels::Other">>, config = [{"base_url", undefined}] })).
