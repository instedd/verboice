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

      def equivalent_flow
          Compiler.make do |compiler|
          compiler.Trace(application_id: @application.id, step_id: @id, step_name: @name, store: '"Message played."')
          compiler.append @message.equivalent_flow if @message
          compiler.append @next.equivalent_flow if @next
        end
      end
    end
  end
end
