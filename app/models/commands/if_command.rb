class Commands::IfCommand < Command
  attr_accessor :condition
  attr_accessor :then
  attr_accessor :else

  def initialize(condition, if_true, if_false = nil)
    @condition = condition
    @then = if_true if if_true
    @else = if_false if if_false
  end

  def next=(cmd)
    @then.last.next = cmd if @then
    @else.last.next = cmd if @else
    super
  end

  def run(session)
    if session.eval(@condition)
      @then || super
    else
      @else || super
    end
  end
end