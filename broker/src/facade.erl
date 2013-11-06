-module(facade).
-export([channel_status/1, notify_call_queued/1, notify_call_queued/2,
  create_channel/2, destroy_channel/2, invalidate_cache/2,
  active_calls_by_channel/1, active_calls_by_project/1, active_calls_by_call_flow/1
]).

channel_status(Channels) ->
  ChannelStatus = dict:fold(fun(Broker, ChannelIds, S) ->
    S ++ Broker:get_channel_status(ChannelIds)
  end, [], Channels),
  Status = [{Id, proplist_to_bert_dict([{ok, Ok}, {messages, Messages}])} || {Id, Ok, Messages} <- ChannelStatus],
  proplist_to_bert_dict(Status).

notify_call_queued(_ChannelId) ->
  scheduler:load(),
  ok.

notify_call_queued(_ChannelId, _NotBefore) ->
  scheduler:load(),
  ok.

create_channel(Id, Broker) ->
  Broker:create_channel(Id).

destroy_channel(Id, Broker) ->
  Broker:destroy_channel(Id).

invalidate_cache(Entity, Id) ->
  cache:delete({Entity, {id, Id}}).

active_calls_by_channel(ChannelId) ->
  session_sup:count({channel, ChannelId}).

active_calls_by_project(ProjectId) ->
  session_sup:count({project, ProjectId}).

active_calls_by_call_flow(CallFlowId) ->
  session_sup:count({call_flow, CallFlowId}).

proplist_to_bert_dict(List) ->
  {bert, dict, proplist_to_bert_dict(List, [])}.

proplist_to_bert_dict([], Dict) -> Dict;
proplist_to_bert_dict([Value | Rest], Dict) ->
  proplist_to_bert_dict(Rest, [Value | Dict]).

