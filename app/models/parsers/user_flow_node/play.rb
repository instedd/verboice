module Parsers
  module UserFlowNode
    class Play < UserCommand
      attr_reader :id, :message, :name, :application

      def initialize application, params
        @id = params['id']
        @name = params['name'] || ''
        @message = Message.for application, self, :message, params['message']
        @application = application
      end

      def equivalent_flow
        [
          @message.equivalent_flow
        ]
      end

    end
  end
end
