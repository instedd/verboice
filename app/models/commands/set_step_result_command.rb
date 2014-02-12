class Commands::SetStepResultCommand < Command
  def initialize(result, data = nil)
    @result = result
    @data = data
  end

  def serialize_parameters
    if @data
      { result: @result, data: @data}
    else
      { result: @result }
    end
  end
end
