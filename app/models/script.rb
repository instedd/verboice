# Convenience class to create command scripts easily instead
# of having to build an array of hashes.
#
# You can use it in two ways.
#
#    script = Script.new do
#      answer
#      play 'something'
#      hangup
#    end
#
# But the previous way won't allow you to include an if command, so
# you can instead do this:
#
#    script = Script.new do |s|
#      s.answer
#      s.play 'something'
#      s.hangup
#    end
#
# In both cases, you can get the array of commands by doing:
#
#    script.commands
class Script
  attr_accessor :commands

  def initialize(&block)
    @commands = []
    if block.arity == 1
      yield self
    else
      instance_eval &block
    end
    @commands
  end

  def puts(something)
    @commands << {:puts => something}
  end

  def method_missing(name, *args)
    if args.length == 0
      @commands << name
    elsif args.length == 1
      @commands << {name => args[0]}
    else
      @commands << {name => args}
    end
  end
end

