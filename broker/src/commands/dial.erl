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

  SuccessAfterSeconds = case proplists:get_value(successful_after, Args) of
                          undefined -> undefined;
                          Expr -> util:parse_short_time(Expr)
                        end,

  poirot:log(info, "Dialing ~s through channel ~s", [Number, Channel#channel.name]),

  DialStart = erlang:now(),

  try
    Result = Pbx:dial(Channel, list_to_binary(Number), CallerId),
    ResultJS = erjs_context:set(dial_status, Result, JS),
    NewJS = maybe_mark_session_successful(DialStart, SuccessAfterSeconds, ResultJS),

    poirot:log(info, "Dial completed with status ~s", [Result]),
    {next, Session#session{js_context = NewJS}}
  catch
    hangup ->
      UpdatedJS = maybe_mark_session_successful(DialStart, SuccessAfterSeconds, JS),
      throw({hangup, Session#session{js_context = UpdatedJS}})
  end.

maybe_mark_session_successful(_DialStart, undefined, JS) ->
  JS;
maybe_mark_session_successful(DialStart, SuccessAfterSeconds, JS) ->
  CallTimeSeconds = timer:now_diff(erlang:now(), DialStart) div 1000000,
  if
    CallTimeSeconds > SuccessAfterSeconds ->
      poirot:log(info, "Call time exceeded threshold, marking session successful", []),
      erjs_context:set(status, "successful", JS);
    true ->
      JS
  end.

