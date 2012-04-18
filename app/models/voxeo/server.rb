module Voxeo
  class Server < EM::Connection
    include EM::HttpServer

    def post_init
      super
      no_environment_strings
    end
    
    def process_http_request
      response = EM::DelegatedHttpResponse.new(self)
      
      fiber = store.get_fiber_for voxeo_session_id
      
      unless fiber
        fiber = Fiber.new do
          BaseBroker.instance.accept_call Voxeo::CallManager.new(channel_id, voxeo_session_id, session_id, caller_id)
        end
        store.store_fiber voxeo_session_id, fiber
      end

      xml = fiber.resume params
      
      response.status = 200
      response.content_type 'text/xml'
      response.content = xml
      response.send_response
    end
    
    private
    
    def params
      @params ||= init_params
    end
    
    def init_params
      @http_query_string.split('&').map{|x|x.split('=')}.inject(HashWithIndifferentAccess.new){|r,x|r[x.first]=x.second;r}
    end
    
    def store
      Voxeo::FiberStore.instance
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
    
  end
end
