class Commands::TraceCommand < Command

  def initialize(options = {})
    @application_id = options[:application_id]
    @step_id = options[:step_id]
    @expression = options[:store]
  end

  def run(session)
    Trace.create! application_id: @application_id, step_id: @step_id, call_id: session.call_id, result: session.eval(@expression)
  end
end