module Asterisk
  class Client < Asterisk::AmiProtocol
    Port = Rails.configuration.asterisk_configuration[:ami_port].to_i

    def post_init
      Fiber.new do
        response = self.login :username => 'verboice', :secret => 'verboice'
        if response[:response] != 'Success'
          puts "Login failed"
          close_connection
        else
          puts response
          $asterisk_client = self
        end
      end.resume
    end

    def unbind
      EM.add_timer(1) do
        EM::connect '127.0.0.1', Port, self.class
      end
      super
    end

    def receive_event(event)
      if event[:event] == 'OriginateResponse' && event[:response] == 'Failure'
        BaseBroker.instance.call_rejected event[:actionid]
      end
    end
  end
end
