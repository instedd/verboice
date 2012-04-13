module Parsers
  module UserFlowNode
    class Menu < UserCommand
      attr_reader :id, :explanation_message, :options, :timeout, :invalid_message, :end_call_message, :name, :application

      def initialize application, params
        @id = params['id']
        @name = params['name'] || ''
        @explanation_message = Message.for application, self, :explanation_message, params['explanation_message']
        @options_message = Message.for application, self, :options_message, params['options_message']
        @options = params['options'].deep_clone || []
        @is_root = params['root'] || false
        @timeout = params['timeout'] || 5
        @number_of_attempts = params['number_of_attempts'] || 3
        @invalid_message = Message.for application, self, :invalid_message, params['invalid_message']
        @end_call_message = Message.for application, self, :end_call_message, params['end_call_message']
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
                :condition => "attempt_number > #{@number_of_attempts} && !end",
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
          a_branch << { assign: { name: 'end', expr: 'true' }}
        else
          a_branch
        end
      end

      def build_capture
        if @options_message
          capture = @options_message.capture_flow
          capture[:timeout] = @timeout
          {
            capture: capture
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
          @equivalent_flow << { :while => { :condition => "attempt_number <= #{@number_of_attempts} && !end", :do =>
            yield << { assign: { name: 'attempt_number', expr: 'attempt_number + 1' }}
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