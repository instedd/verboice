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
    File.join(recordings_folder, "#{step_id}-#{action.to_s.parameterize}.wav")
  end

  def get_result_path_for(step_id)
    File.join(results_folder, "#{step_id}.wav")
  end

  def results_folder
    folder_for_app 'results'
  end

  def recordings_folder
    folder_for_app 'recordings'
  end

  def folder_for_app(folder)
    raise "Cannot create recording manager for non saved application" unless @application.id
    path = File.join Rails.root, "data/applications/#{@application.id}/#{folder}/"
    FileUtils.makedirs(path)
    path
  end

end