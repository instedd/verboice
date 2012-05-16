class Commands::PlayFileCommand < Command
  include Commands::PlayCommand
  param :path, :string, :ui_length => 80

  def initialize(path)
    @file_id = path.sub("#{Rails.root}/data/projects/",'')
  end

  def run(session)
    session.info "Play file #{@file_id}"
    super
  end

  def setup_file(session)
    path = get_target_path(session)
    convert_to_8000_hz_gsm "#{Rails.root}/data/projects/" + @file_id, path
    path
  end
end
