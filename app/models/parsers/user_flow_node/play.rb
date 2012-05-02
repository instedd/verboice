module Parsers
  module UserFlowNode
    class Play < UserCommand
      attr_reader :id, :message, :name, :application
      attr_accessor :next

      def initialize application, params
        @id = params['id']
        @name = params['name'] || ''
        @message = Message.for application, self, :message, params['message']
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
          compiler.Trace context_for '"Message played."'
          compiler.append @message.equivalent_flow if @message
          compiler.append @next.equivalent_flow if @next
        end
      end
    end
  end
end
