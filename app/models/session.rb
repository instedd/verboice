class Session
  attr_accessor :pbx
  attr_accessor :commands
  attr_accessor :application
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
    run_command until @commands.empty?
  end

  def push_commands(commands)
    @commands.unshift *commands
  end

  private

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
