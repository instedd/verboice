class Commands::CallbackCommand < Command
  attr_accessor :url
  attr_accessor :method
  attr_accessor :params

  def initialize(url = nil, options = {})
    @url = url
    @method = options[:method] || 'post'
    @params = options[:params]
    @response_type = options[:response_type] || :flow
    @variables = options[:variables] || {}
  end

  def run(session)
    url = @url || session.callback_url
    url = interpolate_url session, url
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

        f.resume self.send(@response_type, content_type, body)
      rescue Exception => e
        f.resume e
      end
    end
    http.errback { f.resume Exception.new(http.error.present? ? http.error : "Failed to communicate with #{url}") }
    Fiber.yield
  end

  private

  def flow(content_type, body)
     next_command = case content_type
                    when %r(application/json)
                      Commands::JsCommand.new body
                    else
                      Parsers::Xml.parse body
                    end
    next_command.last.next = self.next
    next_command
  end

  def variables(content_type, body)
    hash = JSON.parse body
    next_command = Compiler.make do |c|
      hash.each do |key, value|
        c.Assign key, "'#{value}'"
        c.PersistVariable key, "'#{value}'"
      end
    end

    next_command.last.next = self.next
    next_command
  end

  def none(content_type, body)
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
      session.eval @variables[$1]
    end
  end
end
