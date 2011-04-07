class IfCommand
  def initialize(options = {})
    @condition = options[:condition]
    @then = options[:then]
    @else = options[:else]
  end

  def run(session)
    commands = session.eval(@condition) ? @then : @else
    if commands
      commands = [commands] unless commands.is_a? Array
      session.push_commands commands
    end
  end
end
