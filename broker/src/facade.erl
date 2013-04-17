-module(facade).
-export([channel_status/1, notify_call_queued/1, notify_call_queued/2,
  create_channel/2, destroy_channel/2
]).

channel_status(ChannelIds) ->
  Status = [{Id, proplist_to_bert_dict([{ok,true},{messages,[]}])} || Id <- ChannelIds],
  proplist_to_bert_dict(Status).

notify_call_queued(ChannelId) ->
  ok.

notify_call_queued(ChannelId, NotBefore) ->
  ok.

create_channel(Id, Broker) ->
  ok.

destroy_channel(Id, Broker) ->
  ok.

proplist_to_bert_dict(List) ->
  {bert, dict, proplist_to_bert_dict(List, [])}.

proplist_to_bert_dict([], Dict) -> Dict;
proplist_to_bert_dict([Value | Rest], Dict) ->
  proplist_to_bert_dict(Rest, [Value | Dict]).

