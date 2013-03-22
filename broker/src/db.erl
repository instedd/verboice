-module(db).
-export([select_one/1, select_one/2]).

select_one(Query) -> select_one(Query, []).

select_one(Query, Params) ->
  SQL = format_query(Query, Params),
  {data, Result} = mysql:fetch(db, SQL),
  [Row] = mysql:get_result_rows(Result),
  Row.

format_query(Query, Params) ->
  lists:flatten(io_lib:format(Query, Params)).