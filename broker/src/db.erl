-module(db).
-export([select/1, select/2, select_one/1, select_one/2]).

select(Query) ->
  select(Query, []).

select(Query, Params) ->
  SQL = format_query(Query, Params),
  {data, Result} = mysql:fetch(db, SQL),
  mysql:get_result_rows(Result).

select_one(Query) -> select_one(Query, []).

select_one(Query, Params) ->
  [Row] = select(Query, Params),
  Row.

format_query(Query, Params) ->
  lists:flatten(io_lib:format(Query, Params)).