class CallbackCommand < Command
  param :url, :string, :optional => true

  def initialize(url = nil)
    @url = url
  end

  def run(session)
    url = @url || session.application.callback_url
    body = "CallSid=#{session.id}&Digits=#{session[:last_capture]}"

    session.log :info => "Callback #{url}", :trace => "Callback #{url} with #{body}"

    http = EventMachine::HttpRequest.new(url).post :body => body

    f = Fiber.current
    http.callback do
      body = http.response

      session.trace "Callback returned: #{body}"

      commands = XmlParser.parse body
      session.push_commands commands

      f.resume
    end
    http.errback do
      f.resume Exception.new("Callback failed with status #{http.response_header.status}")
    end
    Fiber.yield
  end
end
