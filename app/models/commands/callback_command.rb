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

    url, authorization = callback_authentication(url, session.application)

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
                     commands = [:js => body]
                   else
                     commands = Parsers::Xml.parse body
                   end

          session.push_commands commands

        f.resume
      rescue Exception => e
        f.resume e
      end
    end
    http.errback { f.resume Exception.new(http.error.present? ? http.error : "Failed to communicate with #{url}") }
    Fiber.yield
  end

  private

  def callback_authentication(url, application)
    uri = URI.parse(url)
    callback_url_user = uri.user || application.callback_url_user
    uri.user = nil

    callback_url_password = uri.password || application.callback_url_password
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
