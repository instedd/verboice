class CallbackCommand
  def initialize(url = nil)
    @url = url
  end

  def run(context)
    @url = context.callback_url unless @url

    http = EventMachine::HttpRequest.new(@url).post :body => "CallSid=#{context.session_id}&Digits=#{context.last_capture}"

    f = Fiber.current
    http.callback do
      body = http.response
      commands = XmlParser.parse body
      context.push commands

      f.resume
    end
    http.errback do
      puts "Error getting #{@url}"
      f.resume
    end
    Fiber.yield
  end
end
