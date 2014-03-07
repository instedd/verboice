-module(hibernated_session).
-define(TABLE_NAME, "hibernated_sessions").
-define(MAP, [
  {data, binary_serializer}
]).
-include_lib("erl_dbmodel/include/model.hrl").
