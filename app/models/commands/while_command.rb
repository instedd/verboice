class WhileCommand
  def initialize(options = {})
    @condition = options[:condition]
    @do = options[:do]
  end

  def run(session)
    if session.eval @condition
      commands = []
      if @do.is_a? Array
        @do.each { |cmd| commands << cmd }
      else
        commands << @do
      end

      commands << self

      session.push_commands commands
    end
  end
end
