module Parsers
  module UserFlowNode
    class Play < UserCommand
      attr_reader :id, :message, :name, :application, :next

      def initialize application, params
        @id = params['id']
        @name = params['name'] || ''
        @message = Message.for application, self, :message, params['message']
        @application = application
        @next = params['next']
        @is_root = params['root'] || false
      end

      def equivalent_flow
        @equivalent_flow || build_equivalent_flow
      end

      def solve_links_with nodes
        # TODO: Test This!!!! A command link mustn't be resolved twice
        if @next && !@next.is_a?(UserCommand)
          possible_nodes = nodes.select do |a_node|
            a_node.id == @next
          end
          if possible_nodes.size == 1
            @next = possible_nodes.first
          else
            if possible_nodes.size == 0
              raise "There is no command with id #{@next}"
            else
              raise "There are multiple commands with id #{@next}: #{possible_nodes.inspect}."
            end
          end
        end
      end

      def is_root?
        @is_root
      end

      def build_equivalent_flow
        @equivalent_flow = @message.equivalent_flow if @message
        @equivalent_flow.next = @next.equivalent_flow if @next
        @equivalent_flow
      end

    end
  end
end
