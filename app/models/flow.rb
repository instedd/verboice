class Flow
  def initialize(context)
    @context = context
  end

  def run(commands)
    commands.each do |cmd|
      if cmd.is_a? Hash
        cmd, args = cmd.first
        cmd = "#{cmd.to_s.camelcase}Command".constantize
        cmd.new(args).run @context
      else
        cmd = "#{cmd.to_s.camelcase}Command".constantize
        cmd.new.run @context
      end
    end
  end
end
