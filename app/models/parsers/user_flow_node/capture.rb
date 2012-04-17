module Parsers
  module UserFlowNode
    class Capture < UserCommand
      attr_reader :id, :name, :next

      def initialize application, params
        @id = params['id']
        @name = params['name'] || ''
        @is_root = params['root'] || false
        @instructions_message = Message.for application, self, :instructions, params['instructions_message']
        @valid_values = params['valid_values']
        @finish_on_key = params['finish_on_key'] || '#'
        @min_input_length = params['min_input_length'] || 1
        @max_input_length = params['max_input_length'] || 1
        @timeout = params['timeout'] || 5
        @number_of_attempts = params['number_of_attempts'] || 3
        @invalid_message = Message.for application, self, :invalid, params['invalid_message']
        @end_call_message = Message.for application, self, :end_call, params['end_call_message']
        @application = application
        @next = params['next']
      end

      def solve_links_with nodes
        if @next && !@next.is_a?(UserCommand)
          possible_nodes = nodes.select do |a_node|
            a_node.id == @next
          end
          if possible_nodes.size == 1
            @next = possible_nodes.first
          else
            if possible_nodes.size == 0
              raise "There is no command with id #{@next}"
            else
              raise "There are multiple commands with id #{@next}: #{possible_nodes.inspect}."
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
        build_while do
          [
            build_capture,
            build_if_conditions
          ]
        end

        if @end_call_message
          @equivalent_flow << if @number_of_attempts > 1
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
        {
          :if => {
            :condition => valid_digits_condition,
            :then => if_must_add_exit_condition_to([trace('"User pressed: " + digits')] +
              if @next
                @next.equivalent_flow
              else
                []
              end
            ),
            :else =>  if @invalid_message
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
          }
        }
      end

      def if_must_add_exit_condition_to a_branch
        if @number_of_attempts > 1
          a_branch << { assign: { name: "end#{@id}", expr: 'true' }}
        else
          a_branch
        end
      end

      def build_capture
        capture = if @instructions_message
          @instructions_message.capture_flow
        else
          []
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

      def valid_digits_condition
        @valid_values.split(/\s*[,;]\s*/).map do |clause|
          items = clause.split(/\s*-\s*/)
          if items.length == 1
            "(digits == #{items.first})"
          else
            "(digits >= #{items.first} && digits <= #{items.last})"
          end
        end.join(' || ')
      end

    end
  end
end