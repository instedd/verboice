class Parsers::UserFlow

  attr_reader :application

  def initialize application, application_flow
    @application_flow = application_flow
    @roots = []
    @nodes = []
    @application = application
  end

  def build_nodes
    @nodes = []

    @application_flow.each do | an_ui_command |
      @nodes << (Parsers::UserFlowNode::UserCommand.for application, an_ui_command)
    end

    @nodes.each do | a_command_parser |
      a_command_parser.solve_links_with @nodes
    end

    @roots = @nodes.select do |a_node|
      a_node.is_root?
    end
  end

  def equivalent_flow
    @equivalent_flow ||= build_equivalent_flow
  end

  def build_equivalent_flow
    build_nodes
    flow = @roots.collect do |a_root_node|
      a_root_node.equivalent_flow
    end
    if flow.size == 1
      flow.first
    else
      flow
    end
  end
end