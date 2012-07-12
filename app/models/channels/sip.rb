class Channels::Sip < Channel

  validate :server_username_uniqueness

  def port
    Rails.configuration.verboice_configuration[:local_pbx_broker_port].to_i
  end

  def asterisk_address_string_for broker, address
    broker.sip_address_string_for self, address
  end

  def server_username_uniqueness
    subclass_responsibility
  end

end