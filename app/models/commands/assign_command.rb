class AssignCommand
  def initialize(options = {})
    @name = options[:name]
    @expr = options[:expr]
  end

  def run(session)
    session[@name.to_s] = session.eval @expr
  end
end
