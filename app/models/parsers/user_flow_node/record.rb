module Parsers
  module UserFlowNode
    class Record < UserCommand
      attr_reader :id, :name, :project
      attr_accessor :next

      def initialize project, params
        @id = params['id']
        @name = params['name'] || ''
        @explanation_message = Message.for project, self, :explanation, params['explanation_message']
        @confirmation_message = Message.for project, self, :confirmation, params['confirmation_message']
        @timeout = params['timeout']
        @stop_key = params['stop_key']
        @project = project
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
          compiler.Trace context_for %("Record message. Download link: " + record_url(#{@id}))
          compiler.append @explanation_message.equivalent_flow
          compiler.Record @id, @name, {:stop_keys => @stop_key, :timeout => @timeout}
          compiler.append @confirmation_message.equivalent_flow
          compiler.append @next.equivalent_flow if @next
        end
      end
    end
  end
end
