-module(facade).
-export([channel_status/1, notify_call_queued/1, notify_call_queued/2,
  create_channel/2, destroy_channel/2, invalidate_cache/2
]).

channel_status(ChannelIds) ->
  Status = [{Id, proplist_to_bert_dict([{ok,true},{messages,[]}])} || Id <- ChannelIds],
  proplist_to_bert_dict(Status).

notify_call_queued(_ChannelId) ->
  scheduler:load(),
  ok.

notify_call_queued(_ChannelId, _NotBefore) ->
  scheduler:load(),
  ok.

create_channel(_Id, _Broker) ->
  ok.

destroy_channel(_Id, _Broker) ->
  ok.

invalidate_cache(Entity, Id) ->
  cache:delete({Entity, {id, Id}}).

proplist_to_bert_dict(List) ->
  {bert, dict, proplist_to_bert_dict(List, [])}.

proplist_to_bert_dict([], Dict) -> Dict;
proplist_to_bert_dict([Value | Rest], Dict) ->
  proplist_to_bert_dict(Rest, [Value | Dict]).

