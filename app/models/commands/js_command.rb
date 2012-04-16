class Commands::JsCommand < Command
  def initialize(source)
    @source = source
  end

  def run(session)
    session.eval @source
    super
  end
end
