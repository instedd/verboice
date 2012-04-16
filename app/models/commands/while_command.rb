class Commands::WhileCommand < Command
  attr_accessor :condition
  attr_accessor :block

  def initialize(condition, block)
    @condition = condition
    @block = block
    block.last.next = self if block
  end

  def run(session)
    if session.eval @condition
      @block
    else
      super
    end
  end
end
