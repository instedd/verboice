class Commands::SetMetadataCommand < Command
  def initialize(metadata)
    @metadata = metadata
  end

  def serialize_parameters
    @metadata
  end
end
