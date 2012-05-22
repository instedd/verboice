module Parsers
  module UserFlowNode
    class Goto < UserCommand
      attr_reader :id, :name, :call_flow
      attr_accessor :next

      def initialize call_flow, params
        @id = params['id']
        @name = params['name'] || ''
        @jump = params['jump']
        @call_flow = call_flow
        @root_index = params['root']
      end

      def solve_links_with nodes
        # There is no need to link to the real node.
      end

      def is_root?
        @root_index.present?
      end

      def root_index
        @root_index
      end

      def equivalent_flow
        Compiler.parse do |compiler|
          compiler.Goto @jump
        end
      end
    end
  end
end