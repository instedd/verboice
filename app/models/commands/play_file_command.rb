class Commands::PlayFileCommand < Command
  include Commands::PlayCommand

  def initialize(path)
    @file_id = path.sub("#{Rails.root}/data/call_flows/",'')
  end

  def run(session)
    session.info "Play file #{@file_id}"
    super
  end

  def setup_file(session)
    path = get_target_path(session)
    convert_to_8000_hz_gsm file_to_convert_path, path
    path
  end

  def should_setup_file?(target_path)
    not File.exists?(target_path) or File.mtime(target_path) < File.mtime(file_to_convert_path)
  end

  def file_to_convert_path
    "#{Rails.root}/data/call_flows/" + @file_id
  end
end
