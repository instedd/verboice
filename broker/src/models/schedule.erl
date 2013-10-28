-module(schedule).
-export([next_available_time/2, load/2]).
-define(CACHE, true).
-define(TABLE_NAME, "schedules").
-define(MAP, [
  {weekdays, {schedule, weekdays}},
  {retries, {schedule, retries}}
]).
-include_lib("erl_dbmodel/include/model.hrl").

next_available_time(Seconds, Schedule) when is_integer(Seconds) ->
  T = calendar:gregorian_seconds_to_datetime(Seconds),
  Offset1 = offset_for_time(T, Schedule),
  Offset2 = offset_for_date(T, Schedule),
  Seconds + Offset1 + Offset2;

next_available_time(T, Schedule) ->
  Offset1 = offset_for_time(T, Schedule),
  Offset2 = offset_for_date(T, Schedule),

  calendar:gregorian_seconds_to_datetime(calendar:datetime_to_gregorian_seconds(T) + Offset1 + Offset2).

offset_for_time({_Date, Time}, #schedule{time_from = {time, From}, time_to = {time, To}}) ->
    FromS = calendar:time_to_seconds(From),
    ToS = calendar:time_to_seconds(To),
    TimeS = calendar:time_to_seconds(Time),

    if
      (TimeS < FromS) and ((TimeS > ToS) or (ToS > FromS)) ->
        FromS - TimeS;
      (TimeS > ToS) and (ToS > FromS) ->
        FromS - TimeS + 86400;
      true -> 0
    end;

offset_for_time(_, #schedule{}) -> 0.

offset_for_date(_, #schedule{weekdays = undefined}) -> 0;
offset_for_date({Date, _Time}, #schedule{weekdays = Weekdays}) ->
  TDay = calendar:day_of_the_week(Date),
  Day = next_weekday(Weekdays, TDay),
  OffsetDays = (Day - TDay) + (if Day < TDay -> 7; true -> 0 end),
  OffsetDays * 86400.

next_weekday(AvailableDays, WDay) ->
  next_weekday(AvailableDays, AvailableDays, WDay).
next_weekday([First|_], [], _) -> First;
next_weekday(_, [D | _], WDay) when D >= WDay-> D;
next_weekday(AvailableDays, [_ | Rest], WDay) -> next_weekday(AvailableDays, Rest, WDay).

load(Weekdays, {schedule, weekdays}) -> parse_weekdays(Weekdays);
load(Retries, {schedule, retries}) -> parse_retries(Retries).

parse_weekdays(undefined) -> undefined;
parse_weekdays(<<>>) -> undefined;
parse_weekdays(Weekdays) ->
  lists:sort([ruby_to_erlang_weekday(util:binary_to_integer(X)) || X <- binary:split(Weekdays, <<",">>, [global])]).

parse_retries(undefined) -> undefined;
parse_retries(<<>>) -> undefined;
parse_retries(Retries) ->
  [binary_to_number(X) || X <- binary:split(Retries, <<",">>, [global])].

ruby_to_erlang_weekday(0) -> 7;
ruby_to_erlang_weekday(N) -> N.

binary_to_number(Bin) ->
  N = binary_to_list(Bin),
  case string:to_float(N) of
    {error, no_float} -> list_to_integer(N);
    {F, _Rest} -> F
  end.
