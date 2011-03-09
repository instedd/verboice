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
# In both cases, doing Script.new will return an array of commands,
# *not* an instance of the Script class.
#
# You can also load a script from the filesystem doing:
#
#    script = Script.load "/foo/bar/path"
class Script < BasicObject
  attr_accessor :commands

  def self.new(&block)
    script = super()
    script.commands = []
    if block.arity == 1
      yield script
    else
      script.instance_eval &block
    end
    script.commands
  end

  def self.load(file)
    new { instance_eval(::File.read file) }
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

