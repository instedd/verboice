module Asterisk
  class AmiClient < AmiProtocol
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
        call_log = CallLog.find(event[:actionid]) or return
        call_log.error 'Failed to establish the communication'
        call_log.finish :failed
      end
    end
  end
end
