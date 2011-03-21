class Session
  attr_accessor :pbx
  attr_accessor :commands
  attr_accessor :application
  attr_accessor :log
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

  def run
    run_command until @commands.empty?
  rescue Exception => ex
    error ex.message
    @log.finish :failed if @log
  else
    @log.finish :completed if @log
  end

  def push_commands(commands)
    @commands.unshift *commands
  end

  def info(text)
    @log.log 'I', text if @log
  end

  def error(text)
    @log.log 'E', text if @log
  end

  def trace(text)
    @log.log 'T', text if @log
  end

  def log(options)
    return unless @log
    if @log_level == :trace
      @log.log 'T', options[:trace]
    else
      @log.log 'I', options[:info]
    end
  end

  def run_command
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
