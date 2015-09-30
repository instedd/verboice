-module(delayed_job).
-define(TABLE_NAME, "delayed_jobs").
-export([enqueue/2, enqueue/3]).
-include_lib("erl_dbmodel/include/model.hrl").

enqueue(Type, Task) ->
  enqueue(Type, Task, 0).

enqueue(Type, Task, DelaySeconds) ->
  NowSeconds = calendar:datetime_to_gregorian_seconds(calendar:universal_time()),
  RunAtSeconds = NowSeconds + DelaySeconds,
  RunAt = {datetime, calendar:gregorian_seconds_to_datetime(RunAtSeconds)},
  Handler = yaml:dump({map, link_with_poirot(Type, Task), type_tag(Type)}, [{schema, yaml_schema_ruby}]),
  create(#delayed_job{handler = Handler, run_at = RunAt}).

type_tag({struct, StructName}) ->
  iolist_to_binary([<<"!ruby/struct:">>, StructName]);
type_tag(ClassName) ->
  iolist_to_binary([<<"!ruby/object:">>, ClassName]).

type_name({struct, StructName}) -> StructName;
type_name(ClassName) -> ClassName.

link_with_poirot(Type, Task) ->
  case poirot:current_id() of
    undefined -> Task;
    ActivityId ->
      LinkId = list_to_binary(uuid:to_string(uuid:v4())),
      poirot:log(info, "Enqueue '~s'", [type_name(Type)], [{link_id, LinkId}]),
      Task ++ [{'_poirot_activity_id', ActivityId}, {'_poirot_link_id', LinkId}]
  end.
