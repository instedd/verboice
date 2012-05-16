module Voxeo
  class Server < EM::Connection
    Port = Rails.configuration.voxeo_configuration[:http_server_port].to_i

    include EM::HttpServer

    def post_init
      super
      no_environment_strings
    end

    def process_http_request
      p "New request, params: #{params.inspect}"

      response = EM::DelegatedHttpResponse.new(self)

      fiber = store.get_fiber_for voxeo_session_id

      unless fiber
        fiber = Fiber.new do |context|
          opts = {:session_id => session_id, :caller_id => caller_id, :context => context}
          BaseBroker.instance.accept_call Voxeo::CallManager.new(channel_id, voxeo_session_id, opts)
        end
        store.store_fiber voxeo_session_id, fiber
      end

      if params[:error]
        xml = fiber.resume Exception.new("Voxeo returned error. Event: #{params[:event]}. Message: #{params[:message]}.")
        p "Error! sending xml: #{xml}"
      elsif params[:disconnect]
        xml = fiber.resume Exception.new("User hung up.")
        p "Disconnect! sending xml: #{xml}"
      else
        xml = fiber.resume context
        p "Sending xml: #{xml}"
      end

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
