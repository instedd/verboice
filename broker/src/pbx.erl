-module(pbx).
-include("db.hrl").

-type pbx() :: any().
-type resource_kind() :: file | url | text.
-type resource() :: {file, string()}
  | {url, string()}
  | {text, string(), string()}.

-callback answer(pbx()) -> any().
-callback hangup(pbx()) -> any().
-callback play(resource(), pbx()) -> any().
-callback capture(any(), any(), any(), any(), any(), pbx()) -> any().
-callback terminate(pbx()) -> any().
-callback can_play(resource_kind(), pbx()) -> boolean().
-callback sound_path_for(string(), pbx()) -> string().
-callback sound_quality(pbx()) -> string().
-callback dial(#channel{}, string(), string(), pbx()) -> completed | busy | no_answer | failed.
-callback pid(pbx()) -> undefined | pid().

-export_type([pbx/0]).
