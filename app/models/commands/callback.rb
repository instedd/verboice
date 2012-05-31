module Commands::Callback

  def callback(url, options = {})
    method = options[:method] || 'post'
    method = method.to_s.downcase
    params = options[:params]
    auth = options[:authentication]

    options = {}
    options[:head] = {'authorization' => [auth[:user], auth[:password]]} if auth
    options[method == 'get' ? :query : :body] = params if params

    http = EventMachine::HttpRequest.new(url).send(method, options)

    f = Fiber.current

    http.callback do
      begin
        if http.response_header.status.to_i != 200
          raise "Callback failed with status #{http.response_header.status}"
        end

        f.resume http
      rescue Exception => e
        f.resume e
      end
    end

    http.errback { f.resume Exception.new(http.error.present? ? http.error : "Failed to communicate with #{url}") }

    Fiber.yield
  end

end
