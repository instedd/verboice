module MagicObjectProtocol

  class Server < EventMachine::Connection
    include EventMachine::Protocols::ObjectProtocol

    def receive_object(obj)
      method, args = obj.first
      self.send(method, *args)
    end
  end

  module Client
    include EventMachine::Protocols::ObjectProtocol

    def method_missing(name, *args)
      send_object name => args
    end
  end

end
