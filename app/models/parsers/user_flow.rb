class Parsers::UserFlow

  def initialize application_flow
    @application_flow = application_flow
    @roots = []
    @nodes = []
  end
  
  def build_nodes
    @nodes = []

    @application_flow.each do | an_ui_command |
      @nodes << (Parsers::UserFlowNode::UserCommand.for an_ui_command)
    end

    @nodes.each do | a_command_parser |
      a_command_parser.solve_links_with @nodes
    end
    
    @roots = @nodes.select do |a_node|
      a_node.is_root?
    end
  end
end