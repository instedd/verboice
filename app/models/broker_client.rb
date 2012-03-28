require_dependency 'pbx_unavailable_exception'

module BrokerClient
  Port = Rails.configuration.verboice_configuration[:broker_port].to_i

  def self.open
    client = EM.connect '127.0.0.1', Port, MagicObjectProtocol::Client
    begin
      yield client
    ensure
      client.close_connection
    end
  end

  def self.method_missing(name, *args)
    open do |client|
      client.send name, *args
    end
  end
end
