class CallbackCommand < Command
  param :url, :string, :optional => true

  def initialize(url = nil)
    @url = url
  end

  def run(session)
    url = @url || session.application.callback_url
    body = {:CallSid => session.id, :Digits => session[:capture]}

    session.log :info => "Callback #{url}", :trace => "Callback #{url} with #{body.to_query}"

    http = EventMachine::HttpRequest.new(url).post :body => body

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
