class Session
  attr_accessor :pbx
  attr_accessor :commands
  attr_accessor :application
  attr_accessor :channel
  attr_accessor :call_log
  attr_reader :id

  def initialize(options = {})
    @vars = {}
    @id = Guid.new.to_s
    @log_level = :trace
    options.each do |key, value|
      send "#{key}=", value
    end
  end

  def []=(key, value)
    @vars[key] = value
  end

  def [](key)
    @vars[key]
  end

  def delete(key)
    @vars.delete key
  end

  def run
    raise "Answering machine detected" if @call_log && @call_log.outgoing? && @pbx.is_answering_machine?

    run_command until @commands.empty?
  rescue Exception => ex
    error ex.message
    @call_log.finish :failed if @call_log
  else
    @call_log.finish :completed if @call_log
  end

  def quit!
    @quit = true
  end

  def push_commands(commands)
    @commands.unshift *commands
  end

  [:info, :error, :trace].each do |name|
    class_eval %Q(
      def #{name}(text)
        @call_log.#{name} text if @call_log
      end
    )
  end

  def log(options)
    return unless @call_log

    if @log_level == :trace
      @call_log.trace options[:trace]
    else
      @call_log.info options[:info]
    end
  end

  def run_command
    raise "Quit" if @quit

    cmd = @commands.shift
    if cmd.is_a? Hash
      cmd, args = cmd.first
      cmd = "#{cmd.to_s.camelcase}Command".constantize.new args
    else
      cmd = "#{cmd.to_s.camelcase}Command".constantize.new
    end

    cmd.run self
  end
end
