class Commands::WhileCommand < Command
  attr_accessor :condition
  attr_accessor :block

  def initialize(condition, block)
    @condition = condition
    @block = block
    if block
      block.last.next = self
    else
      @block = self
    end
  end

  def run(session)
    if session.eval @condition
      @block
    else
      super
    end
  end
end
