module Voxeo
  class Broker < BaseBroker
    
    def call session      
      http = EventMachine::HttpRequest.new(session.channel.url) 
      http = http.get :query => {:tokenid => session.channel.token, :callsid => session.id, :numbertodial => session.address} #TODO AR: we can add a callerid param here
      
      http.callback do
        begin
          if http.response_header.status.to_i != 200
            raise "Voxeo call request failed with status #{http.response_header.status}"
          end
        
          body = http.response

          session.trace "Voxeo call request returned: #{body}"
        
          # Voxeo sends failures as "failure: reason" with http status 200, so...
          if body.start_with? "failure"
            raise "Voxeo call request returned failure, reason: #{body}"
          end
        rescue Exception => e
          session.trace e
          call_request_failed session
        end
      end
      
      http.errback do
        session.trace (http.error.present? ? http.error : "Failed to communicate with Voxeo")
        call_request_failed session
      end
    end
    
    def pbx_available?
      true
    end
    
    def create_channel(channel)
    end

    def delete_channel(channel)
    end
    
    private
    
    def call_request_failed session
      Fiber.new do
        # TODO AR: we can try to set a meaningful reason
        call_rejected session.id, :error
      end.resume
    end
    
  end
end