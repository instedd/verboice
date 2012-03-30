class Session
  attr_accessor :pbx
  attr_accessor :commands
  attr_accessor :application
  attr_accessor :channel
  attr_accessor :call_log
  attr_accessor :address
  attr_accessor :suspended

  delegate :finish_successfully, :finish_with_error, :to => :call_log
  CallLog::Levels.each { |key, name| delegate name, :to => :call_log }

  def initialize(options = {})
    @vars = {}
    @log_level = :trace

    options.each do |key, value|
      send "#{key}=", value
    end
  end

  def id
    @id ||= Guid.new.to_s
  end

  def call_id
    call_log.id
  end

  def js
    @js ||= new_v8_context
  end

  def []=(key, value)
    @vars[key] = value
    js[key.to_s] = value
  end

  def [](key)
    @vars[key]
  end

  def delete(key)
    @vars.delete key
    js[key.to_s] = nil
  end

  def eval(expr)
    js.eval expr.to_s
  end

  def callback_url
    application.callback_url
  end

  def status_callback_url
    application.status_callback_url
  end

  def run
    raise "Answering machine detected" if call_log.outgoing? && pbx.is_answering_machine?

    loop do
      begin
        run_command until commands.empty? || @suspended
      rescue Exception => ex
        raise ex unless @suspended
      end

      break unless @suspended
      @suspend_fiber = Fiber.current
      Fiber.yield
    end
  end

  def suspend
    @suspended = true
  end

  def resume
    @suspended = false
    @suspend_fiber.resume
  end

  def quit!
    @quit = true
  end

  def push_commands(commands)
    self.commands.unshift *commands
  end

  CallLog::Levels.each do |letter, name|
    class_eval %Q(
      def #{name}(text)
        call_log.#{name} text
      end
    )
  end

  def log(options)
    if @log_level == :trace
      call_log.trace options[:trace]
    else
      call_log.info options[:info]
    end
  end

  def run_command
    raise "Quit" if @quit

    cmd = commands.shift

    if cmd.is_a? Hash
      cmd, args = cmd.first
      args.symbolize_keys! if args.is_a? Hash

      cmd = "Commands::#{cmd.to_s.camelcase}Command".constantize.new args
    elsif !cmd.respond_to?(:run)
      cmd = "Commands::#{cmd.to_s.camelcase}Command".constantize.new
    end

    cmd.run self
  end

  def new_v8_context
    ctx = V8::Context.new
    ['digits', 'timeout', 'finish_key'].each { |key| ctx[key] = nil }
    ['answer', 'assign', 'callback', 'capture', 'hangup', 'js', 'play', 'pause', 'record', 'say'].each do |func|
      ctx[func] = lambda do |*options|
        if options.length == 1 && options[0].respond_to?(:to_hash)
          options[0] = options[0].to_hash
          options[0].symbolize_keys!
        end
        "Commands::#{func.camelcase}Command".constantize.new(*options).run self
      end
    end
    ctx['alert'] = lambda { |str| info str }
    ctx
  end

  def notify_status(status)
    if status_callback_url.present?
      status_callback_url_user = application.status_callback_url_user
      status_callback_url_password = application.status_callback_url_password

      authentication = (status_callback_url_user.present? || status_callback_url_password.present?) ? {:head => {'authorization' => [status_callback_url_user, status_callback_url_password]}} : {}

      request = EventMachine::HttpRequest.new status_callback_url
      query = { :CallSid => call_id, :CallStatus => status }
      query[:From] = pbx.caller_id if pbx
      request.get({:query => query}.merge(authentication))
    end
  end
end
