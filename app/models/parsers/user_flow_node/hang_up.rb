module Parsers
  module UserFlowNode
    class HangUp < UserCommand
      attr_reader :id, :name, :application

      def self.can_handle? params
        params['type'] == 'hang_up'
      end

      def initialize application, params
        @id = params['id']
        @name = params['name'] || ''
        @application = application
        @root_index = params['root']
      end

      def solve_links_with nodes
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
          compiler.Trace application_id: @application.id, step_id: @id, step_name: @name, store: '"Application ended call."'
          compiler.End
        end
      end
    end
  end
end
