module Parsers
  class UserFlow

    attr_reader :application

    def initialize application, application_flow
      @application_flow = application_flow
      @roots = []
      @nodes = []
      @application = application
      build_nodes
    end

    def build_nodes
      @nodes = []

      @application_flow.each do | an_ui_command |
        @nodes << (Parsers::UserFlowNode::UserCommand.for application, an_ui_command)
      end

      @nodes.each do | a_command_parser |
        a_command_parser.solve_links_with @nodes
      end

      @roots = @nodes.select(&:is_root?).sort { |a, b| a.root_index <=> b.root_index }
    end

    def equivalent_flow
      @equivalent_flow ||= build_equivalent_flow
    end

    def build_equivalent_flow
      Compiler.make do |compiler|
        compiler.Answer
        @roots.collect do |a_root_node|
          compiler.append a_root_node.equivalent_flow
          compiler.End
        end
      end
    end

    def step_names
      Hash[@nodes.reject do |node|
        node.is_a? UserFlowNode::Goto
      end.collect do |node|
        [node.id, node.name]
      end]
    end
  end
end