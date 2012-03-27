class Commands::WhileCommand
  def initialize(options = {})
    @condition = options[:condition]
    @do = options[:do]
  end

  def run(session)
    if session.eval @condition
      commands = []
      if @do.is_an? Array
        @do.each { |cmd| commands << cmd }
      else
        commands << @do
      end

      commands << self

      session.push_commands commands
    end
  end
end
