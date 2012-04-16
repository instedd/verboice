class Compiler
  attr_accessor :pending_gotos
  attr_accessor :labels

  def initialize
    @labels = {}
    @pending_gotos = Hash.new { |h,k| h[k] = [] }
  end

  def parse &blk
    if block_given?
      if blk.arity == 1
        yield self
      else
        instance_eval &blk
      end
    end

    if @label
      if @last
        @labels[@label] = @last.next
      else
        @labels[@label] = nil
      end
    end

    self
  end

  def make &blk
    parse &blk
    @first
  end

  def self.parse &blk
    Compiler.new.parse &blk
  end

  def self.make &blk
    Compiler.new.make &blk
  end

  def adopt owner, builder
    # Take the 'goto' statements that jump out of the block.
    # Those pointing from the builder itself are at the beginning
    # of the block and will be replaced by the 'owner'
    builder.pending_gotos.each do |label, targets|
      targets.map! { |x| x == builder ? owner : x }
      if @labels[label]
        targets.each do |target|
          target.next = @labels[label]
        end
      else
        @pending_gotos[label].concat targets
      end
    end

    # Adopt the block labels. If the label is defined as 'nil
    # is assumed it is at the end of the block. In that case
    # the label is redefined.
    builder.labels.each do |label, cmd|
      if cmd.nil?
        Label(label)
      else
        (@pending_gotos.delete(label) || []).each do |goto|
          goto.next = cmd
        end
      end
    end
  end

  class NextFaker
    def initialize(target, method)
      @target = target
      @method = method
    end

    def next=(n)
      @target.send(@method, n)
    end
  end

  def If(condition, &blk)
    if_true = Compiler.parse &blk
    cmd = Commands::IfCommand.new(condition, if_true.make)
    append cmd
    adopt NextFaker.new(cmd, :then=), if_true
    self
  end

  def Else(&blk)
    if_false = Compiler.parse &blk
    @last.else = if_false.make
    adopt NextFaker.new(@last, :else=), if_false
    self
  end

  def While(condition, &blk)
    block = Compiler.parse &blk
    cmd = Commands::WhileCommand.new(condition, block.make)

    # Adopt before appending so redefined labels go before the 'while'
    adopt NextFaker.new(cmd, :block=), block
    append cmd
  end

  def Label(name)
    @label = name

    @pending_gotos[@label].each do |goto|
      goto.next = nil
    end
  end

  def Goto(label)
    if @labels[label]
      @last.next = @labels[label]
    else
      @pending_gotos[label] << (@last || self)
    end
  end

  def method_missing(method, *args)
    cmd_class = "Commands::#{method.to_s}Command".constantize
    append cmd_class.new *args
  end

  def next=(cmd)
    @first = cmd
  end

  def append(cmd)
    if @label
      @labels[@label] = cmd

      (@pending_gotos.delete(@label) || []).each do |goto|
        goto.next = cmd
      end

      @label = nil
    end

    @first ||= cmd
    @last.next = cmd if @last
    @last = cmd
    self
  end

end