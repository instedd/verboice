-module(schedule_test).
-include_lib("eunit/include/eunit.hrl").
-include("db.hrl").

time(H, M, S) -> {time, {H, M, S}}.

next_available_time_test_() -> [
  ?LET(Schedule, #schedule{time_from = time(10, 00, 00), time_to = time(14, 00, 00)},
    {"with range during same day", [
      {"returns same value if it falls inside range",
      ?_assertEqual({{2012,05,05},{12,00,00}}, schedule:next_available_time({{2012,05,05},{12,00,00}}, Schedule))},

      {"moves time forward if it falls behind the beginning",
      ?_assertEqual({{2012,05,05},{10,00,00}}, schedule:next_available_time({{2012,05,05},{08,00,00}}, Schedule))},

      {"moves time to next day if it falls after the end",
      ?_assertEqual({{2012,05,06},{10,00,00}}, schedule:next_available_time({{2012,05,05},{15,00,00}}, Schedule))}
    ]}
  ),

  ?LET(Schedule, #schedule{time_from = time(18, 00, 00), time_to = time(05, 00, 00)},
    {"with_range_ending_next_day", [
      {"returns same value if it falls inside range after midnight",
      ?_assertEqual({{2012,05,05},{02,00,00}}, schedule:next_available_time({{2012,05,05},{02,00,00}}, Schedule))},

      {"returns same value if it falls inside range before midnight",
      ?_assertEqual({{2012,05,05},{20,00,00}}, schedule:next_available_time({{2012,05,05},{20,00,00}}, Schedule))},

      {"moves time forward if it falls outside the range",
      ?_assertEqual({{2012,05,05},{18,00,00}}, schedule:next_available_time({{2012,05,05},{10,00,00}}, Schedule))}
    ]}
  ),

  ?LET(Schedule, #schedule{weekdays = [2, 5]},
    {"with weekdays", [
      {"returns same day if current day is in weekdays",
      % tuesday (2)
      ?_assertEqual({{2012,05,01},{00,00,00}}, schedule:next_available_time({{2012,05,01},{00,00,00}}, Schedule))},

      {"returns next day in same week if current day is between weekdays", [
      % wednesday (3)
      ?_assertEqual({{2012,05,04},{00,00,00}}, schedule:next_available_time({{2012,05,02},{00,00,00}}, Schedule)),
      % thursday (4)
      ?_assertEqual({{2012,05,04},{00,00,00}}, schedule:next_available_time({{2012,05,03},{00,00,00}}, Schedule)) ]},

      {"returns next day in next week if current day is after weekdays", [
      % saturday (6)
      ?_assertEqual({{2012,05,08},{00,00,00}}, schedule:next_available_time({{2012,05,05},{00,00,00}}, Schedule)),
      % sunday (7)
      ?_assertEqual({{2012,05,08},{00,00,00}}, schedule:next_available_time({{2012,05,06},{00,00,00}}, Schedule)) ]}
    ]}
  )
].

