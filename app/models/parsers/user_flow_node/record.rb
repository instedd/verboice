module Parsers
  module UserFlowNode
    class Record < UserCommand
      attr_reader :id, :name, :application
      attr_accessor :next

      def initialize application, params
        @id = params['id']
        @name = params['name'] || ''
        @explanation_message = Message.for application, self, :explanation, params['explanation_message']
        @confirmation_message = Message.for application, self, :confirmation, params['confirmation_message']
        @timeout = params['timeout']
        @stop_key = params['stop_key']
        @application = application
        @next = params['next']
        @root_index = params['root']
      end

      def is_root?
        @root_index.present?
      end

      def root_index
        @root_index
      end

      def equivalent_flow
        Compiler.parse do |compiler|
          compiler.Label @id
          compiler.Assign "current_step", @id
          compiler.Trace context_for %("Record message. Download link: #{record_url}")
          compiler.append @explanation_message.equivalent_flow if @explanation_message
          compiler.Record filename, {:stop_keys => @stop_key, :timeout => @timeout}
          compiler.append @confirmation_message.equivalent_flow if @confirmation_message
          compiler.append @next.equivalent_flow if @next
        end
      end

      def filename
        RecordingManager.for(@application).get_result_path_for(@id)
      end

      def record_url
        NamedRoutes.result_application_url(@application.id, :step_id => @id)
      end
    end
  end
end
