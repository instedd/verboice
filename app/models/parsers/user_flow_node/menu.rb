module Parsers
  module UserFlowNode
    class Menu < UserCommand

      attr_reader :id, :explanation_text, :options

      def initialize params
        @id = params[:id]
        @explanation_text = params[:data][:explanation_text]
        @options = params[:data][:options] || []
        @is_root = params[:root] || false
      end

      def solve_links_with nodes
        @options.each do |an_option|
          possible_nodes = nodes.select do |a_node|
            a_node.id == an_option[:next]
          end
          if possible_nodes.size == 1
            an_option[:next] = possible_nodes.first
          else
            if possible_nodes.size == 0
              raise "There is no command with id #{an_option[:next]}"
            else
              raise "There are multiple commands with id #{an_option[:next]}: #{possible_nodes.inspect}."
            end
          end
        end
      end

      def is_root?
        @is_root
      end
    end
  end
end