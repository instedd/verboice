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

class Commands::CallbackCommand < Command
  attr_accessor :url
  attr_accessor :method
  attr_accessor :params

  def initialize(url = nil, options = {})
    @url = url
    @method = options[:method] || 'post'
    @params = options[:params]
  end

  def run(session)
    url = @url || session.callback_url
    method = (@method || 'post').to_s.downcase.to_sym

    url, authorization = callback_authentication(url, session.call_flow)

    body = {:CallSid => session.call_id, :From => session.pbx.caller_id, :Channel => session.channel.name}
    if @params
      @params.each do |name, key|
        body[name] = session[key]
      end
    end
    session.log :info => "Callback #{method} #{url}", :trace => "Callback #{method} #{url} with #{body.to_query}"

    http = http_request(url, method, body, authorization)

    f = Fiber.current
    http.callback do
      begin
        if http.response_header.status.to_i != 200
          raise "Callback failed with status #{http.response_header.status}"
        end

        content_type = http.response_header[EventMachine::HttpClient::CONTENT_TYPE]
        body = http.response

        session.trace "Callback returned #{content_type}: #{body}"

        commands = case content_type
                   when %r(application/json)
                     commands = Commands::JsCommand.new body
                   else
                     commands = Parsers::Xml.parse body
                   end

        commands.last.next = self.next
        f.resume commands
      rescue Exception => e
        f.resume e
      end
    end
    http.errback { f.resume Exception.new(http.error.present? ? http.error : "Failed to communicate with #{url}") }
    Fiber.yield
  end

  private

  def callback_authentication(url, project)
    uri = URI.parse(url)
    callback_url_user = uri.user || project.callback_url_user
    uri.user = nil

    callback_url_password = uri.password || project.callback_url_password
    uri.password = nil

    authorization = {:head => {'authorization' => [callback_url_user, callback_url_password]}} if callback_url_user.present? || callback_url_password.present?

    [uri.to_s, authorization]
  end

  def http_request(url, method, body, authorization = {})
    authorization ||= {}
    request = EventMachine::HttpRequest.new(url)

    if method == :get
      request.get(body.merge(authorization))
    else
      request.post({:body => body}.merge(authorization))
    end
  end
end
