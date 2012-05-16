class Parsers::UserFlowNode::Message

  def self.can_handle? params
    raise "Subclasses must define this message"
  end

  def self.for project, parent_step, action, params
    (SuitableClassFinder.find_direct_subclass_of self, suitable_for: (params || {})).new project, parent_step, action, (params || {})
  end

  def name
    raise "Subclasses must define this message"
  end

  def equivalent_flow
    raise "Subclasses must define this message"
  end

  def capture_flow
    raise "Subclasses must define this message"
  end
end