-module(twiml).
-export([parse/1]).

-include_lib("xmerl/include/xmerl.hrl").

parse(String) ->
  {Xml, _} = xmerl_scan:string(String),
  scan([], Xml).

scan(Flow, Response = #xmlElement{name = 'Response'}) ->
  scan(Flow, Response#xmlElement.content);

scan(Flow, []) -> Flow;

scan(Flow, [#xmlText{} | Rest]) ->
  scan(Flow, Rest);

scan(Flow, [#xmlElement{name = 'Play', content = [#xmlText{value = Url}]} | Rest]) ->
  scan(Flow ++ [[play_url, [{url, Url}]]], Rest);

scan(Flow, [#xmlElement{name = 'Say', content = [#xmlText{value = Text}]} | Rest]) ->
  scan(Flow ++ [[say, [{text, Text}]]], Rest);

scan(Flow, [#xmlElement{name = 'Hangup'} | Rest]) ->
  scan(Flow ++ [hangup], Rest);

scan(Flow, [Pause = #xmlElement{name = 'Pause'} | Rest]) ->
  Cmd = case get_attribute(Pause, length) of
    undefined -> pause;
    Length -> [pause, [{length, list_to_integer(Length)}]]
  end,
  scan(Flow ++ [Cmd], Rest);

scan(Flow, [Redirect = #xmlElement{name = 'Redirect', content = [#xmlText{value = Url}]} | Rest]) ->
  CallbackOpts = case get_attribute(Redirect, method) of
    undefined -> [{url, Url}];
    Method -> [{url, Url}, {method, parse_method(Method)}]
  end,
  scan(Flow ++ [[callback, CallbackOpts]], Rest);

scan(Flow, [Gather = #xmlElement{name = 'Gather'} | Rest]) ->
  CaptureOpts1 =
    lists:foldl(fun(Elem, Opts) ->
      case Elem of
        #xmlElement{name = 'Play', content = [#xmlText{value = Url}]} -> [{play, Url} | Opts];
        #xmlElement{name = 'Say', content = [#xmlText{value = Text}]} -> [{say, Text} | Opts];
        _ -> Opts
      end
    end, [{min, 1}, {max, infinity}], Gather#xmlElement.content),

  {CaptureOpts, CallbackOpts} =
    lists:foldl(fun(Attr, {Opts1, Opts2}) ->
      case Attr#xmlAttribute.name of
        numDigits ->
          N = list_to_integer(Attr#xmlAttribute.value),
          {[{min, N}, {max, N} | proplists:delete(max, proplists:delete(min, Opts1))], Opts2};
        timeout ->
          Timeout = list_to_integer(Attr#xmlAttribute.value),
          {[{timeout, Timeout} | Opts1], Opts2};
        finishOnKey ->
          {[{finish_on_key, Attr#xmlAttribute.value} | Opts1], Opts2};
        action ->
          {Opts1, [{url, Attr#xmlAttribute.value} | Opts2]};
        method ->
          {Opts1, [{method, parse_method(Attr#xmlAttribute.value)} | Opts2]}
      end
    end, {CaptureOpts1, [{params, [{"Digits", "digits"}]}]}, Gather#xmlElement.attributes),

  NextIndex = case Rest of [] -> 3; _ -> 4 end,
  Commands = [
    [capture, CaptureOpts],
    ['if', [{condition, "timeout || finish_key"}, {then, length(Flow) + NextIndex}]],
    [callback, CallbackOpts],
    stop
  ],
  scan(Flow ++ Commands, Rest).

get_attribute(#xmlElement{attributes = Attributes}, AttrName) ->
  case lists:keyfind(AttrName, #xmlAttribute.name, Attributes) of
    #xmlAttribute{value = Value} -> Value;
    _ -> undefined
  end.

parse_method(Str) ->
  case string:to_upper(Str) of
    "GET" -> get;
    "POST" -> post
  end.
