-module(db).
-export([select/1, select/2, select_one/1, select_one/2, insert/1, update/1]).

select(Query) ->
  select(Query, []).

select(Query, Params) ->
  SQL = format_query(Query, Params),
  {data, Result} = mysql:fetch(db, SQL),
  mysql:get_result_rows(Result).

select_one(Query) -> select_one(Query, []).

select_one(Query, Params) ->
  case select(Query, Params) of
    [Row] -> Row;
    [] -> undefined;
    [_|_] -> exit(many_rows_for_select_one)
  end.

insert(Query) ->
  {updated, Result} = mysql:fetch(db, Query),
  mysql:get_result_insert_id(Result).

update(Query) ->
  {updated, Result} = mysql:fetch(db, Query),
  mysql:get_result_affected_rows(Result).

format_query(Query, Params) ->
  io_lib:format(Query, Params).