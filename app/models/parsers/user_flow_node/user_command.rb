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
end