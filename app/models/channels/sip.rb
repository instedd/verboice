class Channels::Sip < Channel
  def port
    Rails.configuration.verboice_configuration[:local_pbx_broker_port].to_i
  end

  def servers
    hosts = config['host'] || []
    servers = []
    hosts.each_with_index do |host, i|
      servers << Server.new(host, config['register'][i], config['direction'][i])
    end
    servers.length == 0 ? [Server.new] : servers
  end

  def asterisk_address_string_for broker, address
    broker.sip_address_string_for self, address
  end

  def self.can_handle? a_kind
    a_kind == 'sip'
  end
end