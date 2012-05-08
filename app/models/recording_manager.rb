class RecordingManager

  def initialize(object)
    @object = object
  end

  def self.for(object)
    self.new(object)
  end

  def save_recording_for(key, action)
    path = recording_path_for(key, action)
    File.open(path,"wb") do |file|
      yield(file)
    end
  end

  def recording_path_for(key, action)
    File.join(recordings_folder, "#{key}-#{action.to_s.parameterize}.wav")
  end

  def result_path_for(key)
    File.join(results_folder, "#{key}.wav")
  end

  def results_folder
    path_for 'results'
  end

  def recordings_folder
    path_for 'recordings'
  end

  def path_for(folder)
    raise "Cannot create recording manager for non saved object" unless @object.id
    path = File.join Rails.root, "data", "#{@object.class.name.underscore.pluralize}", "#{@object.id}", "#{folder}"
    FileUtils.makedirs(path)
    path
  end

end