class IfCommand
  def initialize(options = {})
    @condition = options[:condition]
    @then = options[:then]
    @else = options[:else]
  end

  def run(session)
    commands = session[@condition] ? @then : @else
    session.push_commands Array(commands) if commands
  end
end
