class Commands::JsCommand
  def initialize(source)
    @source = source
  end

  def run(session)
    session.eval @source
  end
end
