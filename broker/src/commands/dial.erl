-module(dial).
-export([run/2]).
-include("session.hrl").
-include("db.hrl").

run(Args, Session = #session{pbx = Pbx, channel = CurrentChannel, call_log = CallLog, js_context = JS}) ->
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

  CallLog:info(["Dialing ", Number, " throug channel ", Channel#channel.name], [{command, "dial"}, {action, "start"}]),

  Result = Pbx:dial(Channel, list_to_binary(Number), CallerId),
  NewJS = erjs_object:set(dial_status, Result, JS),

  CallLog:info(["Dial completed  with status ", atom_to_list(Result)], [{command, "dial"}, {action, "finish"}]),
  {next, Session#session{js_context = NewJS}}.
