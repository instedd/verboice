class Channels::Sip < Channel

  def port
    Rails.configuration.verboice_configuration[:local_pbx_broker_port].to_i
  end

  def asterisk_address_string_for broker, address
    broker.sip_address_string_for self, address
  end
end