# Copyright (C) 2010-2012, InSTEDD
#
# This file is part of Verboice.
#
# Verboice is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# Verboice is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with Verboice.  If not, see <http://www.gnu.org/licenses/>.

module Twilio
  class Server < EM::Connection
    Port = Rails.configuration.twilio_configuration[:http_server_port].to_i

    include EM::HttpServer

    def post_init
      super
      no_environment_strings
    end

    def process_http_request
      response = EM::DelegatedHttpResponse.new(self)

      if @http_request_uri =~ %r(^/audio/(.+)$)
        send_audio(response, $1)
      else
        send_twiml(response)
      end
    end

    def send_twiml(response)
      session = store.session_for twilio_session_id
      fiber = session.get_fiber

      unless fiber
        fiber = Fiber.new do |context|
          opts = {:caller_id => caller_id, :context => context}
          Twilio::Broker.instance.accept_call Twilio::CallManager.new(channel_id, twilio_session_id, opts)
        end
        session.store_fiber fiber
      end

      if params[:error]
        xml = fiber.resume Exception.new("Voxeo returned error. Event: #{params[:event]}. Message: #{params[:message]}.")
      elsif params[:disconnect]
        xml = fiber.resume Exception.new("User hung up.")
      else
        xml = fiber.resume self
      end

      response.status = 200
      response.content_type 'text/xml'
      response.content = xml
      response.send_response
    end

    def send_audio(response, key)
      path = store.session_for(twilio_session_id).get(key)

      begin
        if File.exists? path
          response.status = 200
          response.content_type 'audio/x-gsm'
          File.open path do |f|
            response.content = f.read
          end
        else
          response.status = 404
        end
      rescue Exception => e
        response.status = 500
        response.content = e
      end

      response.send_response
    end

    def params
      @params ||= begin
        params = Rack::Utils.parse_nested_query(@http_query_string)
        params.merge(Rack::Utils.parse_nested_query(@http_content)) if @http && @http[:content_type] =~ %r(application/x-www-form-urlencoded)
        params.with_indifferent_access
      end
    end

    def headers
      @http || {}
    end

    def store
      HttpBroker::SessionStore.instance
    end

    def channel_id
      @channel ||= Channel.find_by_guid params[:channel_guid]
      @channel.id
    end

    def twilio_session_id
      params['CallSid']
    end

    def caller_id
      params['From']
    end
  end
end
