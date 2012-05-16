require_dependency 'pbx_unavailable_exception'

class BrokerClient

  def initialize port
    @port = port
  end

  def open
    client = EM.connect '127.0.0.1', @port, MagicObjectProtocol::Client
    begin
      yield client
    ensure
      client.close_connection
    end
  end

  def method_missing(name, *args)
    open do |client|
      client.send name, *args
    end
  end
end
