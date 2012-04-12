class RecordingManager

  def initialize(app)
    @application = app
  end

  def self.for(app)
    self.new(app)
  end

  def save_recording_for(step_id, action)
    path = get_recording_path_for(step_id, action)
    File.open(path,"wb") do |file|
      yield(file)
    end
  end

  def get_recording_path_for(step_id, action)
    File.join(folder_for_app, "#{step_id}-#{action.to_s.parameterize}.wav")
  end

  def folder_for_app
    raise "Cannot create recording manager for non saved application" unless @application.id
    path = File.join Rails.root, "data/applications/#{@application.id}/recordings/"
    FileUtils.makedirs(path)
    path
  end

end