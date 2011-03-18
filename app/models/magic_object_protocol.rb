module MagicObjectProtocol

  class Server < EventMachine::Connection
    include EventMachine::Protocols::ObjectProtocol

    def receive_object(obj)
      method, args = obj.first
      f = Fiber.new do
        response = self.send(method, *args)
        send_object response
      end
      f.resume
    end
  end

  module Client
    include EventMachine::Protocols::ObjectProtocol

    def method_missing(name, *args)
      @mutex ||= Mutex.new
      @cv ||= ConditionVariable.new
      send_object name => args
      @mutex.synchronize do
        @cv.wait @mutex
      end
      @obj
    end

    def receive_object(obj)
      @obj = obj
      @mutex.synchronize do
        @cv.signal
      end
    end
  end

end
