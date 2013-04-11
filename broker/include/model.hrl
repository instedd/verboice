-include("db.hrl").
-export([create/0, create/1, find/1, find_all/1, update/1, delete/1]).

-ifndef(MAP).
-define(MAP(Record), Record).
-endif.

create() -> create(#?MODULE{}).

create(Record = #?MODULE{}) ->
  Now = {datetime, calendar:universal_time()},
  RecordToInsert = Record#?MODULE{created_at = Now, updated_at = Now},
  Id = db:insert(insert_query(RecordToInsert)),
  RecordToInsert#?MODULE{id = Id}.

find(Id) when is_number(Id) ->
  find({id, Id});

find(Criteria) ->
  Row = db:select_one(select_query(Criteria)),
  Record = list_to_tuple([?MODULE | Row]),
  ?MAP(Record).

find_all(Criteria) ->
  Rows = db:select(select_query(Criteria)),
  lists:map(fun(Row) ->
    Record = list_to_tuple([?MODULE | Row]),
    ?MAP(Record)
  end, Rows).

update(Record = #?MODULE{}) ->
  Now = {datetime, calendar:universal_time()},
  RecordToUpdate = Record#?MODULE{updated_at = Now},
  1 = db:update(update_query(RecordToUpdate)),
  ok.

delete(Record = #?MODULE{}) ->
  1 = db:update(delete_query(Record)),
  ok.

insert_query(Record) ->
  ["INSERT INTO ", ?TABLE_NAME, "(" | insert_fields(Record, record_info(fields, ?MODULE))].

insert_fields(Record, [id | Rest]) ->
  insert_fields(Record, Rest);
insert_fields(Record, [Field]) ->
  [atom_to_list(Field), ") VALUES (" | insert_values(Record, 3, record_info(size, ?MODULE))];
insert_fields(Record, [Field | Rest]) ->
  [atom_to_list(Field), ", " | insert_fields(Record, Rest)].

insert_values(Record, Index, Count) when Index =:= Count ->
  [mysql:encode(element(Index, Record)), ")"];
insert_values(Record, Index, Count) ->
  [mysql:encode(element(Index, Record)), ", " | insert_values(Record, Index + 1, Count)].

select_query(Criteria) ->
  ["SELECT " | select_fields(Criteria, record_info(fields, ?MODULE))].

select_fields(Criteria, [Field]) ->
  [atom_to_list(Field), " FROM ", ?TABLE_NAME, " WHERE " | select_criteria(Criteria)];
select_fields(Criteria, [Field | Rest]) ->
  [atom_to_list(Field), ", " | select_fields(Criteria, Rest)].

select_criteria({Field, Value}) ->
  [atom_to_list(Field), " = ", mysql:encode(Value)];

select_criteria({Field, in, Values}) ->
  [atom_to_list(Field), " IN (", value_list(Values), ")"];

select_criteria([Filter]) ->
  select_criteria(Filter);

select_criteria([Filter | Rest]) ->
  [select_criteria(Filter), " AND " | select_criteria(Rest)].

update_query(Record) ->
  ["UPDATE ", ?TABLE_NAME, " SET " | update_fields(Record, 2, record_info(fields, ?MODULE))].

update_fields(Record, Index, [id | Rest]) ->
  update_fields(Record, Index + 1, Rest);
update_fields(Record, Index, [Field]) ->
  [atom_to_list(Field), " = ", mysql:encode(element(Index, Record)), " WHERE id = ", mysql:encode(Record#?MODULE.id)];
update_fields(Record, Index, [Field | Rest]) ->
  [atom_to_list(Field), " = ", mysql:encode(element(Index, Record)), ", " | update_fields(Record, Index + 1, Rest)].


delete_query(Record) ->
  ["DELETE FROM ", ?TABLE_NAME, " WHERE id = ", mysql:encode(Record#?MODULE.id)].

value_list([]) -> [];
value_list([Value]) ->
  [mysql:encode(Value)];
value_list([Value | Rest]) ->
  [mysql:encode(Value), ", " | value_list(Rest)].
