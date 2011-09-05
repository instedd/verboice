module Asterisk
  class Client < Asterisk::AmiProtocol
    Port = Rails.configuration.asterisk_configuration[:ami_port].to_i

    def post_init
      Fiber.new do
        response = self.login :username => 'verboice', :secret => 'verboice'
        if response[:response] != 'Success'
          puts "Login failed"
        else
          puts response
        end
      end.resume
    end

    def receive_event(event)
      if event[:event] == 'OriginateResponse' && event[:response] == 'Failure'
        BaseBroker.instance.call_rejected event[:actionid]
      end
    end
  end
end
