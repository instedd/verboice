module Voxeo
  class Server < EM::Connection
    Port = Rails.configuration.voxeo_configuration[:http_server_port].to_i
    
    include EM::HttpServer

    def post_init
      super
      no_environment_strings
    end

    def process_http_request
      response = EM::DelegatedHttpResponse.new(self)
      
      fiber = store.get_fiber_for voxeo_session_id
      
      unless fiber
        fiber = Fiber.new do |context|
          opts = {:session_id => session_id, :caller_id => caller_id, :context => context}
          BaseBroker.instance.accept_call Voxeo::CallManager.new(channel_id, voxeo_session_id, opts)
        end
        store.store_fiber voxeo_session_id, fiber
      end

      xml = fiber.resume context
      
      response.status = 200
      response.content_type 'text/xml'
      response.content = xml
      response.send_response
    end
    
    private
    
    def context
      @context ||= Voxeo::HttpContext.new @http_headers, @http_query_string
    end
    
    def params
      context.params
    end
    
    def store
      Voxeo::FiberStore.instance
    end
    
    def channel_id
      @channel ||= Channel.find_by_guid params[:channel_guid]
      @channel.id
    end
    
    def voxeo_session_id
      params['session.sessionid'] || params['sessionid']
    end
    
    def session_id
      params[:callsid]
    end
    
    def caller_id
      params['session.callerid']
    end
    
  end
end
