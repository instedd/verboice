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

  include JavascriptUtils

  def initialize(url = nil, options = {})
    @url = url
    @method = options[:method] || 'post'
    @params = options[:params]
    @response_type = options[:response_type] || :flow
    @variables = options[:variables] || {}
    @external_service_guid = options[:external_service_guid]
  end

  def run(session)
    last_entry = session.call_log.last_entry

    session.info "Callback started", command: 'callback', action: 'start'
    url = @url || session.callback_url
    url = interpolate_url session, url
    method = (@method || 'post').to_s.downcase.to_sym

    url, authorization = callback_authentication(url, session.call_flow)

    body = {:CallSid => session.call_id, :From => session.pbx.caller_id, :Channel => session.channel.name}
    body[:LastEntry] = last_entry.id if last_entry.present?

    @params.each do |name, key|
      body[name] = session[key]
    end if @params

    @variables.each do |name, expr|
      body[name] = session.eval expr
    end if @variables

    session.trace "Callback #{method} #{url} with #{body.to_query}", command: 'callback', action: method

    http = http_request(url, method, body, authorization)

    f = Fiber.current
    http.callback do
      begin
        if http.response_header.status.to_i != 200
          session.trace "Callback status #{http.response_header.status}", command: 'callback', action: 'fail'
          raise "Callback failed with status #{http.response_header.status}"
        end

        content_type = http.response_header[EventMachine::HttpClient::CONTENT_TYPE]
        body = http.response

        session.trace "Callback returned #{content_type}: #{body}", command: 'callback', action: 'return'

        f.resume self.send(@response_type, content_type, body, session)
      rescue Exception => e
        session.error "#{e}", command: 'callback', action: 'error'
        f.resume e
      end
    end
    http.errback do
      error_message = http.error.present? ? http.error : "Failed to communicate with #{url}"
      session.trace error_message, command: 'callback', action: 'error'
      f.resume Exception.new(error_message)
    end
    session.info "Callback ended", command: 'callback', action: 'finish'
    Fiber.yield
  end

  private

  def flow(content_type, body, session)
     next_command = case content_type
                    when %r(application/json)
                      Commands::JsCommand.new body
                    else
                      Parsers::Xml.parse body
                    end
    next_command.last.next = self.next
    next_command
  end

  def variables(content_type, body, session)
    hash = JSON.parse body
    hash.each do |key, value|
      session["response_#{key}"] = session.eval(value_for_js(value))
    end
    self.next
  end

  def none(content_type, body, session)
    self.next
  end

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

  def interpolate_url(session, url)
    url.gsub(/\{([^\{]*)\}/) do
      if @variables[$1].present?
        session.eval @variables[$1]
      else
        value = session["var_#{$1}"]
        value = external_service(session).global_variable_value_for $1 unless value
        value
      end
    end
  end

  def external_service session
    @external_service ||= session.call_flow.project.external_services.find_by_guid(@external_service_guid)
  end

end
