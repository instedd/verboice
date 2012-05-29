module Parsers
  module UserFlowNode
    class HangUp < UserCommand
      attr_reader :id, :name, :call_flow

      def self.can_handle? params
        params['type'] == 'hang_up'
      end

      def initialize call_flow, params
        @id = params['id']
        @name = params['name'] || ''
        @call_flow = call_flow
        @root_index = params['root']
      end

      def next
        # There is no next after hang up
        nil
      end

      def next= command
        # There is no next after hang up
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
          compiler.Trace context_for '"Verboice ended call."'
          compiler.End
        end
      end
    end
  end
end
