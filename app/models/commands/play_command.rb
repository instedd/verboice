module Commands::PlayCommand
  
  include AudioUtils

  def initialize(file_id)
    @file_id = file_id
  end

  def run(session)
    target_path = download session
    session.pbx.play target_path
    super
  end

  def download(session)
    target_path = get_target_path(session)
    if File.exists? target_path
      session.trace "File #{target_path} already exists"
    else
      setup_file(session)
    end
    target_path
  end

  def get_target_path(session)
    @target_path ||= session.pbx.sound_path_for @file_id
  end

  def setup_file(session)
    raise "#{self.class.name} must implement setup_file"
  end
  
end
