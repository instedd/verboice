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
    _log 'I', text
  end

  def error(text)
    _log 'E', text
  end

  def trace(text)
    _log 'T', text
  end

  def log(options)
    if @log_level == :trace
      _log 'T', options[:trace]
    else
      _log 'I', options[:info]
    end
  end

  private

  def _log(level, text)
    @log.details << "#{level} #{Time.now.utc - @log.created_at} #{text}\n" if @log
    @log.details_will_change!
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
