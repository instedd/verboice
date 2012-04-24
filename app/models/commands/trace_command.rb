class Commands::TraceCommand < Command

  def initialize(options = {})
    @application_id = options[:application_id]
    @step_id = options[:step_id]
    @step_name = options[:step_name]
    @expression = options[:store]
  end

  def run(session)
    Trace.create!\
      application_id: @application_id,
      step_id: session.eval(@step_id),
      step_name: @step_name,
      call_id: session.call_id,
      result: session.eval(@expression)
    super
  end
end