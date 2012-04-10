module Parsers
  module UserFlowNode
    class Menu < UserCommand

      attr_reader :id, :explanation_text, :options, :timeout, :invalid_text, :end_call_text

      def initialize params
        @id = params['id']
        @explanation_text = params['explanation_text']
        @options_text = params['options_text']
        @options = params['options'] || []
        @is_root = params['root'] || false
        @timeout = params['timeout'] || 5
        @number_of_attempts = params['number_of_attempts'] || 3
        @invalid_text = params['invalid_text']
        @end_call_text = params['end_call_text']
      end

      def solve_links_with nodes
        @options.each do |an_option|
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

      def is_root?
        @is_root
      end

      def equivalent_flow
         @equivalent_flow ||= build_equivalent_flow
      end

      def build_equivalent_flow
        @equivalent_flow = []
        @equivalent_flow << {say: @explanation_text} if @explanation_text
        if @options.empty?
          @equivalent_flow << { say: @options_text } if @options_text
        else
          build_while do
            [build_capture,
            build_if_conditions]
          end
        end

        if @end_call_text
          @equivalent_flow << if @number_of_attempts > 1 && !@options.empty?
            {
              :if => {
                :condition => "attempt_number > #{@number_of_attempts} && !end",
                :then => [{ say: @end_call_text }]
              }
            }
          else
            { say: @end_call_text }
          end
        end

        @equivalent_flow
      end

      private

      def build_if_conditions
       if_conditions = []
        @options.each do |an_option|
          if_conditions << {
            :if => {
              :condition => "digits == #{an_option['number']}",
              :then => if_must_add_exit_condition_to(an_option['next'].equivalent_flow)
            }
          }
        end
        last_if_condition = if_conditions.pop
        last_if_condition[:if][:else] =
         {
           :if => {
              :condition => "digits != null",
              :then => [{ say: @invalid_text }]
            }
          } if @invalid_text
        if_conditions.reverse.each do |an_if_condition_hash|
          an_if_condition_hash[:if][:else] = last_if_condition
          last_if_condition = an_if_condition_hash
        end
        last_if_condition
      end

      def if_must_add_exit_condition_to a_branch
        if @number_of_attempts > 1
          a_branch << { assign: { name: 'end', expr: 'true' }}
        else
          a_branch
        end
      end

      def build_capture
        if @options_text
          {
            capture: {
              timeout: @timeout,
              say: @options_text
            }
          }
        else
          {
            capture: {
              timeout: @timeout
            }
          }
        end
      end

      def build_while
        if @number_of_attempts > 1
          @equivalent_flow << { assign: { name: 'attempt_number', expr: '1' }}
          @equivalent_flow << { assign: { name: 'end', expr: 'false' }}
          @equivalent_flow << { :while => { :condition => "attempt_number <= #{@number_of_attempts} && !end", :do => [
            yield,
            { assign: { name: 'attempt_number', expr: 'attempt_number + 1' }}
          ].flatten}}
        else
          @equivalent_flow << yield
          @equivalent_flow.flatten!
        end
      end
    end
  end
end