module Parsers
  module UserFlowNode
    class EmptyMessage < Message

      attr_reader :name

      def self.can_handle? params
        params.empty?
      end

      def initialize project, parent_step, action, params
        @project = project
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