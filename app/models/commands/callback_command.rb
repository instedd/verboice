class CallbackCommand < Command
  param :url, :string, :optional => true
  param :method, :string, :optional => true, :default => 'post'

  def initialize(options = {})
    if options.is_a? String
      @url = options
    else
      @url = options[:url]
      @method = options[:method]
    end
  end

  def run(session)
    url = @url || session.callback_url
    method = (@method || 'post').to_s.downcase.to_sym

    body = {:CallSid => session.call_id, :Digits => session[:digits]}
    session.log :info => "Callback #{method} #{url}", :trace => "Callback #{method} #{url} with #{body.to_query}"

    request = EventMachine::HttpRequest.new(url)
    http = method == :get ? request.get(body) : request.post(:body => body)

    f = Fiber.current
    http.callback do
      begin
        if http.response_header.status.to_i != 200
          raise "Callback failed with status #{http.response_header.status}"
        end

        body = http.response

        session.trace "Callback returned: #{body}"

        commands = XmlParser.parse body
        session.push_commands commands

        f.resume
      rescue Exception => e
        f.resume e
      end
    end
    http.errback { f.resume Exception.new(http.error) }
    Fiber.yield
  end
end
