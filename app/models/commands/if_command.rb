class IfCommand
  def initialize(options = {})
    @condition = options[:condition]
    @then = options[:then]
    @else = options[:else]
  end

  def run(session)
    session.push_commands(Array(session[@condition] ? @then : @else))
  end
end
