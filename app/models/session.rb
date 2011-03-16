class Session
  attr_accessor :pbx
  attr_accessor :commands
  attr_accessor :application
  attr_accessor :log
  attr_reader :id

  def initialize(options = {})
    @vars = {}
    @id = Guid.new.to_s
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
    p "!!!"
    run_command until @commands.empty?
    p "???"
  rescue => ex
    p ":-("
    @log.finish :failed if @log
  else
    p ":-D"
    @log.finish :completed if @log
  end

  def push_commands(commands)
    @commands.unshift *commands
  end

  def info(text)
    log 'I', text
  end

  def error(text)
    log 'E', text
  end

  private

  def log(level, text)
    @log.details << "#{level} #{Time.now.utc.to_s} #{text}\n" if @log
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
