module Parsers
  module UserFlowNode
    class Branch < UserCommand
      attr_reader :id, :options, :name, :project
      attr_accessor :next

      def initialize project, params
        @id         = params['id']
        @name       = params['name'] || ''
        @options    = params['options'].deep_clone || []
        @root_index = params['root']
        @project    = project
        @next       = params['next']
      end

      def solve_links_with nodes
        @options.each do |an_option|
          an_option['next'] = node_linked_by an_option['next'], nodes
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
            "(#{first_comparison_term_from condition} #{condition['operator']} #{second_comparison_term_from condition})"
          end.join(' && ')
        end
      end

      def first_comparison_term_from condition
        condition['step'].presence ? "value_#{condition['step']}" : "var_#{condition['variable']}"
      end

      def second_comparison_term_from condition
        condition['rhs_variable'].presence ? "var_#{condition['rhs_variable']}" : "#{condition['value']}"
      end

      def retrieve_variables compiler, conditions
        return if conditions.nil?
        conditions.collect do |condition|
          ['variable', 'rhs_variable'].collect do |var_name|
            condition[var_name].presence
          end
        end.flatten.compact.uniq.each do |variable|
          compiler.RetrieveVariable variable
        end
      end
    end
  end
end