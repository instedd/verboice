-module(util_test).
-include_lib("eunit/include/eunit.hrl").

parse_short_time_test() ->
  ?assertEqual(15, util:parse_short_time("15s")),
  ?assertEqual(15, util:parse_short_time(" 15 seconds ")),
  ?assertEqual(15 * 60, util:parse_short_time("15m")),
  ?assertEqual(15 * 60, util:parse_short_time(" 15 minutes ")),
  ?assertEqual(15 * 60 * 60, util:parse_short_time("15h")),
  ?assertEqual(15 * 60 * 60, util:parse_short_time(" 15 hours ")),
  ?assertEqual(15 * 60 * 60 * 24, util:parse_short_time("15d")),
  ?assertEqual(15 * 60 * 60 * 24, util:parse_short_time(" 15 days ")),
  ?assertEqual(15 * 60 * 60, util:parse_short_time("15")),
  ?assertEqual({unexpected_short_time, "hello"}, catch util:parse_short_time("hello")),
  ?assertEqual({unexpected_short_time, ""}, catch util:parse_short_time("")).
