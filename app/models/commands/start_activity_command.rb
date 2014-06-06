class Commands::StartActivityCommand < Command
  def initialize(name, metadata = nil)
    @name = name
    @metadata = metadata
  end

  def serialize_parameters
    params = { name: @name }
    params[:metadata] = @metadata if @metadata
    params
  end
end
