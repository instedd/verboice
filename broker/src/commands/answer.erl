-module(answer).
-export([run/1]).
-include("session.hrl").

run(#session{pbx = Pbx, call_log = CallLog}) ->
  CallLog:info("Answer", [{command, "answer"}, {action, "start"}]),
  Pbx:answer(),
  next.