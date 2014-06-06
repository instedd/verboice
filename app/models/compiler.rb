# Copyright (C) 2010-2012, InSTEDD
#
# This file is part of Verboice.
#
# Verboice is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# Verboice is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with Verboice.  If not, see <http://www.gnu.org/licenses/>.

class Compiler
  attr_accessor :first
  attr_accessor :labels
  attr_accessor :variables
  attr_accessor :external_service_guids
  attr_accessor :resource_guids

  def initialize
    @labels = {}
    @variables = Set.new
    @external_service_guids = Set.new
    @resource_guids = Set.new
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

  def PersistVariable(variable, expression)
    @variables.add variable unless ImplicitVariable.find(variable)
    append Commands::PersistVariableCommand.new(variable, expression)
  end

  def Callback url=nil, options={}
    @external_service_guids.add options[:external_service_guid] if options[:external_service_guid].present?
    append Commands::CallbackCommand.new(url, options)
  end

  def PlayResource id, language=nil
    # if options[:guid].present?
    #   @resource_guids << options[:guid]
    # elsif options[:id].present?
      # @resource_guids << Resource.find(options[:id]).guid
    # end

    append Commands::PlayResourceCommand.new(id, language)
  end

  def Assign(*args)
    append Commands::AssignExpressionCommand.new(*args)
  end

  def StartUserStep(type, id, name, metadata = {})
    metadata[:step_type] = type
    metadata[:step_id] = id
    StartActivity name, metadata
  end

  def SetStepResult(result, data = nil)
    md = { step_result: result }
    md[:step_data] = [:eval, data] if data
    SetMetadata md
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
      @variables.merge block.variables
      @external_service_guids.merge block.external_service_guids
      @resource_guids.merge block.resource_guids
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
