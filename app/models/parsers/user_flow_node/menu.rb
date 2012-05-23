module Parsers
  module UserFlowNode
    class Menu < UserCommand
      attr_reader :id, :explanation_message, :options, :timeout, :invalid_message, :end_call_message, :name, :project
      attr_accessor :next

      def initialize project, params
        @id                      = params['id']
        @name                    = params['name'] || ''
        @explanation_message     = Message.for project, self, :explanation, params['explanation_message']
        @options_message         = Message.for project, self, :options, params['options_message']
        @options                 = params['options'].deep_clone || []
        @root_index              = params['root']
        @timeout                 = params['timeout'] || self.class.default_time_out_in_seconds
        @number_of_attempts      = params['number_of_attempts'] || self.class.default_number_of_attempts
        @invalid_message         = Message.for project, self, :invalid, params['invalid_message']
        @default                 = params['default']
        @project                 = project
        @next                    = params['next']
        @persisted_variable_name = params['store']
      end

      def solve_links_with nodes
        @options.each do |an_option|
          an_option['next'] = node_linked_by an_option['next'], nodes
        end
        @default = node_linked_by @default, nodes
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
          c.append @explanation_message.equivalent_flow
          c.Assign "attempt_number#{@id}", '1'
          c.While "attempt_number#{@id} <= #{@number_of_attempts}" do |c|
            c.Capture({finish_on_key: '', timeout: @timeout}.merge(@options_message.capture_flow))
            c.Assign "value_#{@id}", 'digits'
            c.PersistVariable @persisted_variable_name, "value_#{@id}" if @persisted_variable_name
            @options.each do |an_option|
              c.If "digits == '#{an_option['number']}'" do |c|
                c.Trace context_for '"User pressed: " + digits'
                c.append an_option['next'].equivalent_flow if an_option['next']
                c.Goto "end#{@id}"
              end
            end
            c.If "digits != null" do |c|
              c.append @invalid_message.equivalent_flow
              c.Trace context_for '"Invalid key pressed"'
            end
            c.Else do |c|
              c.Trace context_for '"No key was pressed. Timeout."'
            end
            c.Assign "attempt_number#{@id}", "attempt_number#{@id} + 1"
          end
          c.Trace context_for %("Missed input for #{@number_of_attempts} times.")
          c.append @default.equivalent_flow if @default
          c.Label "end#{@id}"
          c.append @next.equivalent_flow if @next
        end
      end

      def self.default_number_of_attempts
        3
      end

      def self.default_time_out_in_seconds
        Commands::CaptureCommand.default_time_out_in_seconds
      end
    end
  end
end