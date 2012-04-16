module Parsers
  module UserFlowNode
    class Menu < UserCommand
      attr_reader :id, :explanation_message, :options, :timeout, :invalid_message, :end_call_message, :name, :application

      def initialize application, params
        @id = params['id']
        @name = params['name'] || ''
        @explanation_message = Message.for application, self, :explanation, params['explanation_message']
        @options_message = Message.for application, self, :options, params['options_message']
        @options = params['options'].deep_clone || []
        @is_root = params['root'] || false
        @timeout = params['timeout'] || 5
        @finish_on_key = params['finish_on_key'] || '#'
        @min_input_length = params['min_input_length'] || 1
        @max_input_length = params['max_input_length'] || 1
        @number_of_attempts = params['number_of_attempts'] || 3
        @invalid_message = Message.for application, self, :invalid, params['invalid_message']
        @end_call_message = Message.for application, self, :end_call, params['end_call_message']
        @application = application
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
         @equivalent_flow || build_equivalent_flow
      end

      def build_equivalent_flow
        @equivalent_flow = []
        @equivalent_flow << @explanation_message.equivalent_flow if @explanation_message
        if @options.empty?
          @equivalent_flow << @options_message.equivalent_flow if @options_message
        else
          build_while do
            [
              build_capture,
              build_if_conditions
            ]
          end
        end

        if @end_call_message
          @equivalent_flow << if @number_of_attempts > 1 && !@options.empty?
            {
              :if => {
                :condition => "attempt_number#{@id} > #{@number_of_attempts} && !end#{@id}",
                :then => [@end_call_message.equivalent_flow, trace("\"Missed input for #{@number_of_attempts} times.\"")]
              }
            }
          else
            @end_call_message.equivalent_flow
          end
        end
        @equivalent_flow << trace("\"Call ended.\"")
        @equivalent_flow
      end

      private

      def build_if_conditions
       if_conditions = []
        @options.each do |an_option|
          if_conditions << {
            :if => {
              :condition => "digits == #{an_option['number']}",
              :then => [trace('"User pressed: " + digits')] + if_must_add_exit_condition_to(an_option['next'].equivalent_flow)
            }
          }
        end
        last_if_condition = if_conditions.pop
        last_if_condition[:if][:else] = if @invalid_message
         {
           :if => {
              :condition => "digits != null",
              :then => [@invalid_message.equivalent_flow, trace('"Invalid key pressed"')],
              :else => [trace('"No key was pressed. Timeout."')]
            }
          }
        else
          {
            :if => {
              :condition => "digits != null",
              :then => [trace('"Invalid key pressed"')],
              :else => [trace('"No key was pressed. Timeout."')]
            }
          }
        end
        if_conditions.reverse.each do |an_if_condition_hash|
          an_if_condition_hash[:if][:else] = last_if_condition
          last_if_condition = an_if_condition_hash
        end
        last_if_condition
      end

      def if_must_add_exit_condition_to a_branch
        if @number_of_attempts > 1
          a_branch << { assign: { name: "end#{@id}", expr: 'true' }}
        else
          a_branch
        end
      end

      def build_capture
        capture = if @options_message
          @options_message.capture_flow
        else
          {}
        end
        capture[:timeout] = @timeout
        capture[:min] = @min_input_length
        capture [:max] = @max_input_length
        capture [:finish_on_key] = @finish_on_key
        {
          capture: capture
        }
      end

      def build_while
        if @number_of_attempts > 1
          @equivalent_flow << { assign: { name: "attempt_number#{@id}", expr: '1' }}
          @equivalent_flow << { assign: { name: "end#{@id}", expr: 'false' }}
          @equivalent_flow << { :while => { :condition => "attempt_number#{@id} <= #{@number_of_attempts} && !end#{@id}", :do =>
            yield << { assign: { name: "attempt_number#{@id}", expr: "attempt_number#{@id} + 1" }}
          }}
        else
          @equivalent_flow + yield
        end
      end

      def trace expression
        {
          trace: {
            :application_id => @application.id,
            :step_id => @id,
            :step_name => @name,
            :store => expression
          }
        }
      end
    end
  end
end