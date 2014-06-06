-module(dial).
-export([run/2]).
-include("session.hrl").
-include("db.hrl").

run(Args, Session = #session{pbx = Pbx, channel = CurrentChannel, js_context = JS}) ->
  Number = proplists:get_value(number, Args),
  CallerId = proplists:get_value(caller_id, Args),

  Channel = case proplists:get_value(channel_name, Args) of
    undefined -> CurrentChannel;
    ChannelName ->
      AccountId = CurrentChannel#channel.account_id,
      case channel:find([{account_id, AccountId}, {name, ChannelName}]) of
        undefined -> exit(channel_not_found);
        NewChannel ->
          CurrentBroker = channel:broker(CurrentChannel),
          case channel:broker(NewChannel) of
            CurrentBroker -> NewChannel;
            _ -> exit(different_broker)
          end
      end
  end,

  poirot:log(info, "Dialing ~s throug channel ~s", [Number, Channel#channel.name]),

  Result = Pbx:dial(Channel, list_to_binary(Number), CallerId),
  NewJS = erjs_context:set(dial_status, Result, JS),

  poirot:log(info, "Dial completed with status ~s", [Result]),
  {next, Session#session{js_context = NewJS}}.
