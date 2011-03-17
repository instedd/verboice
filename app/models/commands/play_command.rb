class PlayCommand < Command
  param :url, :string, :ui_length => 80

  def initialize(url)
    @url = url
  end

  def run(session)
    target_path = download session

    session.info "Play #{@url}"
    session.pbx.play target_path
  end

  def download(session)
    @md5 = Digest::MD5.hexdigest @url
    target_path = session.pbx.sound_path_for @md5
    if File.exists? target_path
      session.trace "File #{@url} is already downloaded"
    else
      session.trace "Download #{@url}"
      download_url_to target_path
    end
    target_path
  end

  private

  def download_url_to(target_path)
    download_url_to_temporary_location do |file|
      convert_to_wav file if File.is_mpeg? file
      convert_to_8000_hz_gsm file, target_path
    end
  end

  def download_url_to_temporary_location
    tmp_file = File.new "#{Rails.root}/tmp/#{@md5}", "wb"

    http = EventMachine::HttpRequest.new(@url).get
    http.stream{|chunk| tmp_file.print chunk}

    f = Fiber.current
    http.callback do
      begin
        tmp_file.flush

        yield tmp_file.path

        File.delete tmp_file
        f.resume
      rescue Exception => e
        f.resume e
      end
    end
    Fiber.yield
  end

  def convert_to_wav(file)
    FileUtils.mv file, "#{file}.mp3"
    `lame --decode #{file}.mp3 #{file}.wav`
    File.delete "#{file}.mp3"
    FileUtils.mv "#{file}.wav", file
  end

  def convert_to_8000_hz_gsm(input, output)
    `sox #{input} -r 8000 -c1 #{output}`
    if $?.exitstatus == 2
      raise Exception.new 'Error processing audio file'
    end
  end
end
