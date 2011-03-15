class Flow
  def initialize(context)
    @context = context
  end

  def run(commands)
    class << @context
      attr_accessor :commands
      attr_accessor :session_id
      def push(more_commands)
        @commands.unshift *more_commands
      end
    end

    @context.commands = commands
    @context.session_id = Guid.new.to_s

    run_command commands.shift until commands.empty?
  end

  private

  def run_command(cmd)
    if cmd.is_a? Hash
      cmd, args = cmd.first
      cmd = "#{cmd.to_s.camelcase}Command".constantize.new args
    else
      cmd = "#{cmd.to_s.camelcase}Command".constantize.new
    end

    cmd.run @context
  end
end

