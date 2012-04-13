module Parsers
  module UserFlowNode
    class Text < Message

      attr_reader :name

      def self.can_handle? params
        params['type'] == 'text'
      end

      def initialize application, parent_step, action, params
        @application = application
        @parent = parent_step
        @name = params['name']
      end

      def equivalent_flow
        { say: @name }
      end
    end
  end
end