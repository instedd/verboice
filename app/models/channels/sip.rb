class Channels::Sip < Channel
  validate :server_username_uniqueness
  config_accessor :number

  def port
    Rails.configuration.verboice_configuration[:local_pbx_broker_port].to_i
  end

  def asterisk_address_string_for broker, address
    broker.sip_address_string_for self, address
  end

  def server_username_uniqueness
    subclass_responsibility
  end

  def errors_count
    status = broker_client.channel_status(id)[id]
    status && !status[:ok] ? status[:messages].length : 0
  end
end