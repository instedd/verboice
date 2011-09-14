module PlayCommand

  def initialize(file_id)
    @file_id = file_id
  end

  def run(session)
    target_path = get_target_path(session)
    if File.exists? target_path
      session.trace "File #{target_path} already exists"
    else
      setup_file(session)
    end
    session.pbx.play target_path
  end

  def get_target_path(session)
    @target_path ||= session.pbx.sound_path_for @file_id
  end

  def setup_file(session)
    raise "#{self.class.name} must implement setup_file"
  end

  private

  def convert_to_wav(file)
    FileUtils.mv file, "#{file}.mp3"
    `lame --decode #{file}.mp3 #{file}.wav`
    File.delete "#{file}.mp3"
    FileUtils.mv "#{file}.wav", file
  end

  def convert_to_8000_hz_gsm(input, output)
    new_input = File.is_wav?(input) ? "#{input}.wav" : "#{input}.gsm"
    FileUtils.mv input, new_input
    `sox #{new_input} -r 8000 -c1 #{output}`
    FileUtils.mv new_input, input
    if $?.exitstatus == 2
      raise Exception.new 'Error processing audio file'
    end
  end
end
