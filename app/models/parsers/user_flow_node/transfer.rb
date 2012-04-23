module Parsers
  module UserFlowNode
    class Transfer < UserCommand
      attr_reader :id, :name, :application, :next

      def initialize application, params
        @id = params['id']
        @name = params['name'] || ''
        @address = params['address']
        @channel = params['channel']
        @application = application
        @next = params['next']
        @root_index = params['root']
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
        @root_index.present?
      end

      def root_index
        @root_index
      end

      def equivalent_flow
        channel_name = @channel.present? ? @channel : 'current channel'
        Compiler.parse do |compiler|
          compiler.Label @id
          compiler.Trace application_id: @application.id, step_id: @id, step_name: @name, store: %("Transfer to #{@address} in channel #{channel_name}.")
          compiler.Dial @address, {:channel => @channel}
          compiler.append @next.equivalent_flow if @next
        end
      end
    end
  end
end
