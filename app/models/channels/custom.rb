class Channels::Custom < Channel
  def port
    Rails.configuration.verboice_configuration[:local_pbx_broker_port].to_i
  end

  def asterisk_address_string_for broker, address
    broker.custom_address_string_for self, address
  end

  def self.can_handle? a_kind
    a_kind == 'custom'
  end
end