class Commands::StartUserStepCommand < Command
  def initialize(type, id, name, metadata = nil)
    @type = type
    @id = id
    @name = name
    @metadata = metadata
  end

  def serialize_parameters
    { type: @type, id: @id, name: @name }.tap do |params|
      params[:metadata] = @metadata if @metadata
    end
  end
end
