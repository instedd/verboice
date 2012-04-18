class Compiler
  attr_accessor :first
  attr_accessor :labels

  def initialize
    @labels = {}
  end

  def parse &blk
    if block_given?
      if blk.arity == 1
        yield self
      else
        instance_eval &blk
      end
    end
    self
  end

  def make &blk
    parse &blk
    resolve_gotos
    @first
  end

  def self.parse &blk
    Compiler.new.parse &blk
  end

  def self.make &blk
    Compiler.new.make &blk
  end

  def If(condition, &blk)
    append Commands::IfCommand.new(condition, inner_block(&blk))
  end

  def Else(&blk)
    @last.else = inner_block(&blk)
    self
  end

  def While(condition, &blk)
    append Commands::WhileCommand.new(condition, inner_block(&blk))
  end

  def Label(name)
    label = Label.new(name)
    @labels[name] = label
    append label
  end

  def Goto(label)
    append Goto.new(label)
  end

  def method_missing(method, *args)
    cmd_class = "Commands::#{method.to_s}Command".constantize
    append cmd_class.new *args
  end

  def append(cmd)
    @first ||= cmd
    @last.next = cmd if @last
    @last = cmd
    self
  end

  private

  def resolve_gotos(parent = self, var = :@first, visited = Set.new)
    node = parent.instance_variable_get var
    return unless node

    if node.is_a? Label
      parent.instance_variable_set var, node.next
      return resolve_gotos parent, var, visited
    elsif node.is_a? Goto
      parent.instance_variable_set var, @labels[node.label].next
      return resolve_gotos parent, var, visited
    end

    # Avoid infinite loops
    return if visited.include? node
    visited.add node

    node.instance_variables.each do |var|
      val = node.instance_variable_get var
      if val.is_a? Command
        resolve_gotos node, var, visited
      end
    end
  end

  def inner_block(&blk)
    compiler = Compiler.parse(&blk)
    @labels.merge! compiler.labels
    compiler.first
  end

  class CompilerCommand < Command
    def run(session)
      raise "Unexpected command. This command (#{self}) should not be in the flow. Did you forget to call the 'make' method?"
    end
  end

  class Goto < CompilerCommand
    attr_accessor :label

    def initialize(label)
      @label = label
    end
  end

  class Label < CompilerCommand
    attr_accessor :name

    def initialize(name)
      @name = name
    end
  end

end