class Parsers::UserFlowNode::UserCommand

  def self.can_handle? params
    params['type'] == name.downcase[name.rindex('::') + 2 ... name.size]
  end

  def self.for call_flow, params
    (SuitableClassFinder.find_direct_subclass_of self, suitable_for: params).new call_flow, params
  end

  def equivalent_flow
    raise "Subclasses must define this message"
  end

  def is_root?
    raise "Subclasses must define this message"
  end

  def root_index
    raise "Subclasses must define this message"
  end

  def solve_links_with
    raise "Subclasses must define this message"
  end

  def id
    raise "Subclasses must define this message"
  end

  def call_flow
    raise "Subclasses must define this message"
  end

  def name
    raise "Subclasses must define this message"
  end

  def next
    raise "Subclasses must define this message"
  end

  def next= a_node
    raise "Subclasses must define this message"
  end

  def context_for message
    {
      call_flow_id: call_flow.id,
      step_id: id,
      step_name: name,
      store: message
    }
  end

  def solve_links_with nodes
    self.next = node_linked_by self.next, nodes
  end

  def node_linked_by id, nodes
    if id && !id.is_a?(Parsers::UserFlowNode::UserCommand)
      possible_nodes = nodes.select do |a_node|
        a_node.id == id
      end
      if possible_nodes.size == 1
        possible_nodes.first
      else
        if possible_nodes.size == 0
          raise "There is no command with id #{id}"
        else
          raise "There are multiple commands with id #{id}: #{possible_nodes.inspect}."
        end
      end
    else
      id
    end
  end
end