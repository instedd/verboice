class BrokerFacade < MagicObjectProtocol::Server
  Port = Rails.configuration.verboice_configuration[:broker_port].to_i

  def notify_call_queued(channel_id)
    channel = Channel.find channel_id
    BaseBroker.instance.notify_call_queued channel
  end

  def create_channel(channel_id)
    channel = Channel.find channel_id
    BaseBroker.instance.create_channel channel
  end

  def delete_channel(channel_id)
    channel = Channel.find channel_id
    BaseBroker.instance.delete_channel channel
  end
end
