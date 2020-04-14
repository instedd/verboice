-module(dial).
-export([run/2]).
-include("session.hrl").
-include("db.hrl").

run(Args, Session = #session{pbx = Pbx, channel = CurrentChannel, js_context = JS, call_log = CallLog, contact = Contact, project = Project}) ->
  RawNumber = proplists:get_value(number, Args),
  CallerId = proplists:get_value(caller_id, Args),
  Number = util:interpolate_js(util:as_binary(RawNumber), JS),

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

  Key = util:to_string(proplists:get_value(key, Args)),
  Description = proplists:get_value(description, Args),

  [LocalFilename, AsteriskFilename] = case proplists:get_value(record_call, Args) of
    true -> CallLogId = CallLog:id(),
            recording_utils:asterisk_and_local_filenames(CallLogId, Key);
    _ -> [undefined, undefined]
  end,

  poirot:log(info, "Dialing ~s through channel ~s", [Number, Channel#channel.name]),

  DialStart = erlang:now(),

  try
    Result = Pbx:dial(Channel, Number, CallerId, LocalFilename, AsteriskFilename),
    ResultJS = erjs_context:set(dial_status, Result, JS),
    NewJS = maybe_mark_session_successful(DialStart, SuccessAfterSeconds, ResultJS),

    poirot:log(info, "Dial completed with status ~s", [Result]),
    {next, Session#session{js_context = NewJS}}
  catch
    hangup ->
      UpdatedJS = maybe_mark_session_successful(DialStart, SuccessAfterSeconds, JS),
      throw({hangup, Session#session{js_context = UpdatedJS}})
  after
    persist_recording(AsteriskFilename, [Contact, Project, CallLog:id(), Key, Description])
  end.

persist_recording(undefined, _) -> undefined;
persist_recording(_AsteriskFilename, [Contact, Project, CallLogId, Key, Description]) -> 
  RecordedAudio = #recorded_audio{
    contact_id = Contact#contact.id,
    project_id = Project#project.id,
    call_log_id = CallLogId,
    key = Key,
    description = Description
  },
  RecordedAudio:save().

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
