class Parsers::UserFlowNode::UserCommand

  def self.can_handle? params
    params['type'] == name.downcase[name.rindex('::') + 2 ... name.size]
  end

  def self.for application, params
    (SuitableClassFinder.find_direct_subclass_of self, suitable_for: params).new application, params
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

  def application
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
      application_id: application.id,
      step_id: id,
      step_name: name,
      store: message
    }
  end

  def solve_links_with nodes
    if self.next && !self.next.is_a?(Parsers::UserFlowNode::UserCommand)
      possible_nodes = nodes.select do |a_node|
        a_node.id == self.next
      end
      if possible_nodes.size == 1
        self.next = possible_nodes.first
      else
        if possible_nodes.size == 0
          raise "There is no command with id #{self.next}"
        else
          raise "There are multiple commands with id #{self.next}: #{possible_nodes.inspect}."
        end
      end
    end
  end
end