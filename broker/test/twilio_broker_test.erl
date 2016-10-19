-module(twilio_broker_test).
-include_lib("eunit/include/eunit.hrl").

parse_exception_test() ->
  XmlString = "<?xml version='1.0' encoding='UTF-8'?>\n<TwilioResponse><RestException><Code>13223</Code><Message>The phone number you are attempting to call, 12345, is not valid.</Message><MoreInfo>https://www.twilio.com/docs/errors/13223</MoreInfo><Status>400</Status></RestException></TwilioResponse>",
  ?assertEqual({error, "The phone number you are attempting to call, 12345, is not valid.", "twilio:13223"}, twilio_broker:parse_exception("", XmlString)).
