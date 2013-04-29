-module(pbx).
-export([behaviour_info/1]).

behaviour_info(callbacks) -> [{answer, 1}, {hangup, 1}, {play, 2}, {capture, 6}, {terminate, 1}, {sound_path_for, 2}];
behaviour_info(_) -> undefined.
