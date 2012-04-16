module Voxeo
  class Server < EM::Connection
    include EM::HttpServer
    
    @@fibers = {}

    def post_init
      super
      no_environment_strings
    end
    
    def process_http_request
      response = EM::DelegatedHttpResponse.new(self)
      
      if is_new?
        f = Fiber.new do
          BaseBroker.instance.accept_call Voxeo::CallManager.new(channel_id, voxeo_session_id, session_id, caller_id)
        end
        store_fiber f
      else
        f = stored_fiber
      end

      xml = f.resume params
      
      response.status = 200
      response.content_type 'text/xml'
      response.content = xml
      response.send_response
        
      # TODO AR how do we delete the fiber? memory leak
      # delete_fiber unless data[:continue]
    end
    
    private
    
    def params
      @params ||= init_params
    end
    
    def init_params
      @http_query_string.split('&').map{|x|x.split('=')}.inject(HashWithIndifferentAccess.new){|r,x|r[x.first]=x.second;r}
    end
    
    def is_new?
      stored_fiber.nil?
    end
    
    def stored_fiber
      @@fibers[voxeo_session_id]
    end
    
    def store_fiber fiber
      @@fibers[voxeo_session_id] = fiber
    end
    
    def channel_id
      Channel.first.id
    end
    
    def voxeo_session_id
      params['session.sessionid']
    end
    
    def session_id
      params[:callsid]
    end
    
    def caller_id
      params['session.callerid']
    end
    
    # TODO AR what happens if we never get here? we need some kind of timer
    def delete_fiber
      @@fibers.delete(voxeo_session_id).resume
    end
    
  end
end
