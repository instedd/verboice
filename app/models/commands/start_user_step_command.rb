class Commands::StartUserStepCommand < Command
  def initialize(type, id, name)
    @type = type
    @id = id
    @name = name
  end

  def serialize_parameters
    { type: @type, id: @id, name: @name }
  end
end
