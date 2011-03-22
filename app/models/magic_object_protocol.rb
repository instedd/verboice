module MagicObjectProtocol

  class Server < EventMachine::Connection
    include EventMachine::Protocols::ObjectProtocol

    def receive_object(obj)
      method, args = obj.first
      f = Fiber.new do
        begin
          response = self.send(method, *args)
          send_object response
        rescue Exception => ex
          send_object ex
        end
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
        @cv.wait @mutex unless @obj
      end
      raise @obj if @obj.is_a? Exception
      @obj
    end

    def receive_object(obj)
      @mutex.synchronize do
        @obj = obj
        @cv.signal
      end
    end

    def unbind
      @mutex.synchronize do
        @obj = Exception.new 'Cannot connect to PBX'
        @cv.signal
      end
    end
  end

end
