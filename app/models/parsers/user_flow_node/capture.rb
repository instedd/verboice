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
         @equivalent_flow ||= build_equivalent_flow
      end

      def build_equivalent_flow
        Compiler.make do |compiler|
          compiler.Assign("attempt_number#{@id}", '1')
            .While "attempt_number#{@id} <= #{@number_of_attempts}" do |compiler|
              compiler.Capture({
                min: @min_input_length, max: @max_input_length, finish_on_key: @finish_on_key, timeout: @timeout
              }.merge(@instructions_message.capture_flow))
                .If(valid_digits_condition) do |compiler|
                  compiler.Trace(application_id: @application.id, step_id: @id, step_name: @name, store: '"User pressed: " + digits')
                    .Goto "end#{@id}"
                end
                .If("digits != null") do |compiler|
                  compiler.append(@invalid_message.equivalent_flow)
                  .Trace application_id: @application.id, step_id: @id, step_name: @name, store: '"Invalid key pressed"'
                end
                .Else do |compiler|
                  compiler.Trace application_id: @application.id, step_id: @id, step_name: @name, store: '"No key was pressed. Timeout."'
                end
                .Assign "attempt_number#{@id}", "attempt_number#{@id} + 1"
            end
            .Trace(application_id: @application.id, step_id: @id, step_name: @name, store: %("Missed input for #{@number_of_attempts} times."))
            .append(@end_call_message.equivalent_flow)
          compiler.End
          compiler.Label("end#{@id}")
          compiler.append(@next.equivalent_flow) if @next
        end
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
