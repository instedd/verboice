class Commands::AssignCommand < Command
  attr_accessor :name
  attr_accessor :expr

  def initialize(name, expr)
    @name = name
    @expr = expr
  end

  def run(session)
    session[@name.to_s] = session.eval @expr
    super
  end
end
