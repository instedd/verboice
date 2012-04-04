module Commands::PlayCommand

  def initialize(file_id)
    @file_id = file_id
  end

  def run(session)
    target_path = download session
    session.pbx.play target_path
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

  def download_url_to(target_path)
    download_url_to_temporary_location do |file|
      convert_to_wav file if File.is_mpeg? file
      convert_to_8000_hz_gsm file, target_path
    end
  end

  def download_url_to_temporary_location
    tmp_file = File.new "#{Rails.root}/tmp/#{@file_id}.#{Random.rand(1000000000)}", "wb"

    http = EventMachine::HttpRequest.new(@url).get
    http.stream { |chunk| tmp_file.print chunk }

    f = Fiber.current
    http.callback do
      begin
        if http.response_header.status.to_i != 200
          raise "Download failed with status #{http.response_header.status}"
        end

        tmp_file.flush

        yield tmp_file.path

        File.delete tmp_file
        f.resume
      rescue Exception => e
        f.resume e
      end
    end
    http.errback { f.resume Exception.new(http.error) }
    Fiber.yield
  end
end
