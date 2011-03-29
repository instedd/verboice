module PbxClient

  def self.open
    port = Rails.configuration.verboice_configuration[:pbx_interface_port].to_i
    client = EM.connect '127.0.0.1', port, MagicObjectProtocol::Client
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
