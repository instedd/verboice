module Parsers
  module UserFlowNode
    class Goto < UserCommand
      attr_reader :id, :message, :name, :application, :next

      def initialize application, params
        @id = params['id']
        @name = params['name'] || ''
        @application = application
        @next = params['next']
        @root_index = params['root']
      end

      def solve_links_with nodes
        # There is no need to link to the actual node.
      end

      def is_root?
        @root_index.present?
      end

      def root_index
        @root_index
      end

      def equivalent_flow
        Compiler.parse do |compiler|
          compiler.Goto @next
        end
      end
    end
  end
end