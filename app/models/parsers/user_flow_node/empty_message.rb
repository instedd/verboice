module Parsers
  module UserFlowNode
    class EmptyMessage < Message

      attr_reader :name

      def self.can_handle? params
        params.empty?
      end

      def initialize call_flow, parent_step, action, params
        @call_flow = call_flow
        @parent = parent_step
        @name = params['name']
      end

      def equivalent_flow
        nil
      end

      def capture_flow
        {}
      end
    end
  end
end