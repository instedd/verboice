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

  def If(condition, if_true = nil, &blk)
    append Commands::IfCommand.new(condition, inner_block(if_true, &blk))
  end

  def Else(block = nil, &blk)
    @last.else = inner_block(block, &blk)
    self
  end

  def While(condition, block = nil, &blk)
    append Commands::WhileCommand.new(condition, inner_block(block, &blk))
  end

  def Label(name)
    label = Label.new(name)
    @labels[name] = label
    append label
  end

  def Goto(label)
    append Goto.new(label)
  end

  def End
    Goto nil
  end

  def method_missing(method, *args)
    cmd_class = "Commands::#{method.to_s}Command".constantize
    append cmd_class.new *args
  end

  def append(cmd)
    if cmd
      cmd = inner_block(cmd) if cmd.is_a? Compiler

      @first ||= cmd
      @last.next = cmd if @last
      @last = cmd.last
    end
    self
  end

  private

  def resolve_gotos
    nodes_to_visit = []
    visited_nodes = Set.new
    nodes_to_visit.push parent: self, node: first, variable: :@first

    while !nodes_to_visit.empty? do

      context = nodes_to_visit.shift

      parent = context[:parent]
      node = context[:node]
      variable = context[:variable]

      next unless node

      if node.is_a? Label
        parent.instance_variable_set variable, node.next
        nodes_to_visit.push parent: parent, node: node.next, variable: variable
      elsif node.is_a? Goto
        next_node = if node.label
          label_node = @labels[node.label]
          if label_node
            label_node.next
          else
            raise "Unmatched Goto label #{node.label}"
          end
        else
          nil
        end
        parent.instance_variable_set variable, next_node
        nodes_to_visit.push parent: parent, node: next_node, variable: variable
      elsif ! visited_nodes.include? node
        visited_nodes.add node

        node.instance_variables.each do |var|
          val = node.instance_variable_get var
          if val.is_a? Command
            nodes_to_visit.push parent: node, node: val, variable: var
          end
        end
      end
    end
  end

  def inner_block(block = nil, &blk)
    block ||= Compiler.parse(&blk)
    if block.is_a? Compiler
      @labels.merge! block.labels
      block.first
    else
      block
    end
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