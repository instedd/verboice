module Parsers
  module UserFlowNode
    class Capture < UserCommand
      attr_reader :id, :name, :application
      attr_accessor :next

      def initialize application, params
        @id = params['id']
        @name = params['name'] || ''
        @root_index = params['root']
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
          c.Assign "attempt_number#{@id}", '1'
          c.While "attempt_number#{@id} <= #{@number_of_attempts}" do |c|
            c.Capture({
                min: @min_input_length,
                max: @max_input_length,
                finish_on_key: @finish_on_key,
                timeout: @timeout
              }.merge( @instructions_message.capture_flow ))
            c.Assign "value_#{@id}", 'digits'
            c.If valid_digits_condition do |c|
              c.Trace context_for '"User pressed: " + digits'
              c.Goto "end#{@id}"
            end

            invalid_message_block = lambda { |c|
              c.append @invalid_message.equivalent_flow
              c.Trace context_for '"Invalid key pressed"'
            }

            if @min_input_length == 0
              c.Else &invalid_message_block
            else
              unless @valid_values.blank?
                c.If "digits != null", &invalid_message_block
              end
              c.Else do |c|
                c.Trace context_for '"No key was pressed. Timeout."'
              end
            end
            c.Assign "attempt_number#{@id}", "attempt_number#{@id} + 1"
          end
          c.Trace context_for %("Missed input for #{@number_of_attempts} times.")
          c.append @end_call_message.equivalent_flow
          c.End
          c.Label "end#{@id}"
          c.append @next.equivalent_flow if @next
        end
      end

      def valid_digits_condition
        if @valid_values && !@valid_values.blank?
          conditions = @valid_values.split(/\s*[,;]\s*/).map do |clause|
            items = clause.split(/\s*-\s*/)
            if items.length == 1
              "(digits == #{items.first})"
            else
              "(digits >= #{items.first} && digits <= #{items.last})"
            end
          end
          conditions << '(digits == null)' if @min_input_length == 0
          conditions.join(' || ')
        else
          'true'
        end
      end

    end
  end
end
