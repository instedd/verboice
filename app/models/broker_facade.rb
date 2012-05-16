class BrokerFacade < MagicObjectProtocol::Server

  def notify_call_queued(channel_id, not_before = nil)
    if not_before
      BaseBroker.instance.schedule_call not_before
    else
      channel = Channel.find channel_id
      BaseBroker.instance.notify_call_queued channel
    end
    nil
  end

  def create_channel(channel_id)
    channel = Channel.find channel_id
    BaseBroker.instance.create_channel channel
  end

  def delete_channel(channel_id)
    channel = Channel.find channel_id
    BaseBroker.instance.delete_channel channel
  end

  def active_calls_count_for(channel_id)
    channel = Channel.find channel_id
    BaseBroker.instance.active_calls_count_for channel
  end

  def redirect(session_id, options)
    BaseBroker.instance.redirect session_id, options
  end

end
