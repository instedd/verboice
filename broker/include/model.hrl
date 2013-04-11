-include("db.hrl").
-export([create/0, create/1, find/1, update/1, delete/1]).

create() -> create(#?MODULE{}).

create(Record = #?MODULE{}) ->
  Now = {datetime, calendar:universal_time()},
  RecordToInsert = Record#?MODULE{created_at = Now, updated_at = Now},
  Id = db:insert(insert_query(RecordToInsert)),
  RecordToInsert#?MODULE{id = Id}.

find(Id) ->
  Row = db:select_one(select_query(Id)),
  list_to_tuple([?MODULE | Row]).

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

select_query(Id) ->
  ["SELECT " | select_fields(Id, record_info(fields, ?MODULE))].

select_fields(Id, [Field]) ->
  [atom_to_list(Field), " FROM ", ?TABLE_NAME, " WHERE id = ", mysql:encode(Id)];
select_fields(Id, [Field | Rest]) ->
  [atom_to_list(Field), ", " | select_fields(Id, Rest)].

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
