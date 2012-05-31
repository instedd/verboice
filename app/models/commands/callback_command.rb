class Commands::CallbackCommand < Command
  include Commands::Callback

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
    url, authentication = callback_authentication(url, session.call_flow)
    method = @method.to_s.downcase.to_sym

    callback_options = {}
    callback_options[:authentication] = authentication
    callback_options[:method] = method
    callback_options[:params] = {:CallSid => session.call_id, :From => session.pbx.caller_id, :Channel => session.channel.name}
    if @params
      @params.each do |name, key|
        callback_options[:params][name] = session[key]
      end
    end

    session.log :info => "Callback #{method} #{url}", :trace => "Callback #{method} #{url} with #{callback_options[:params].to_query}"

    http = callback(url, callback_options)

    content_type = http.response_header[EventMachine::HttpClient::CONTENT_TYPE]
    body = http.response

    session.trace "Callback returned #{content_type}: #{body}"

    next_command =  case content_type
                    when %r(application/json)
                      Commands::JsCommand.new body
                    else
                      Parsers::Xml.parse body
                    end

    next_command.last.next = self.next

    next_command
  end

  private

  def callback_authentication(url, project)
    uri = URI.parse(url)
    callback_url_user = uri.user || project.callback_url_user
    uri.user = nil

    callback_url_password = uri.password || project.callback_url_password
    uri.password = nil

    authentication = {:user => callback_url_user, :password => callback_url_password} if callback_url_user.present? || callback_url_password.present?

    [uri.to_s, authentication]
  end

end
