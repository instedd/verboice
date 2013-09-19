-module(ami_events).
-export([start_link/0, add_handler/2, notify_event/1]).

start_link() ->
  gen_event:start_link({local, ?MODULE}).

add_handler(Handler, Args) ->
  gen_event:add_handler(?MODULE, Handler, Args).

notify_event(Event) ->
  gen_event:notify(?MODULE, Event).