-module(agi_events).
-export([start_link/0, add_handler/2, add_sup_handler/2, notify_new_session/2]).

start_link() ->
  gen_event:start_link({local, ?MODULE}).

add_handler(Handler, Args) ->
  gen_event:add_handler(?MODULE, Handler, Args).

add_sup_handler(Handler, Args) ->
  gen_event:add_sup_handler(?MODULE, Handler, Args).

notify_new_session(Pid, Params) ->
  gen_event:notify(?MODULE, {new_session, Pid, Params}).
