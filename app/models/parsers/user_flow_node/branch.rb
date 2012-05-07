module Parsers
  module UserFlowNode
    class Branch < UserCommand
      attr_reader :id, :options, :name, :application
      attr_accessor :next

      def initialize application, params
        @id = params['id']
        @name = params['name'] || ''
        @options = params['options'].deep_clone || []
        @root_index = params['root']
        @application = application
        @next = params['next']
      end

      def solve_links_with nodes
        @options.each do |an_option|
          if an_option['next'] && !an_option['next'].is_a?(UserCommand)
            possible_nodes = nodes.select do |a_node|
              a_node.id == an_option['next']
            end
            if possible_nodes.size == 1
              an_option['next'] = possible_nodes.first
            else
              if possible_nodes.size == 0
                raise "There is no command with id #{an_option['next']}"
              else
                raise "There are multiple commands with id #{an_option['next']}: #{possible_nodes.inspect}."
              end
            end
          end
        end
        super
      end

      def is_root?
        @root_index.present?
      end

      def root_index
        @root_index
      end

      def equivalent_flow
        Compiler.parse do |c|
          c.Label @id
          c.Assign "current_step", @id
          @options.each_with_index do |an_option, index|
            retrieve_variables c, an_option['conditions']
            c.If(merge_conditions_from(an_option['conditions'])) do |c|
              c.Trace context_for "\"Branch number #{index + 1} selected: '#{an_option['description']}'\""
              c.append(an_option['next'].equivalent_flow) if an_option['next']
              c.Goto("end#{@id}")
            end
          end
          c.Trace context_for '"No branch was selected."'
          c.Label("end#{@id}")
          c.append(@next.equivalent_flow) if @next
        end
      end

      def merge_conditions_from conditions
        if conditions.nil? or conditions.empty?
          'true'
        else
          conditions.collect do |condition|
            "(#{condition['step'].presence ? "value_#{condition['step']}" : "var_#{condition['variable']}"} #{condition['operator']} #{condition['value']})"
          end.join(' && ')
        end
      end

      def retrieve_variables compiler, conditions
        unless conditions.nil?
          conditions.collect do |condition|
            compiler.RetrieveVariable condition['variable'] if condition['variable'].presence
          end
        end
      end

    end
  end
end