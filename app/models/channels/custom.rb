class Channels::Custom < Channel

  validates_numericality_of :limit, :only_integer => true, :greater_than => 0, :if => :has_limit?

  config_accessor :dial_string
  config_accessor :limit

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