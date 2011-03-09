class PlayCommand
  def initialize(url)
    @url = url
  end

  def run(context)
    target_path = download context
    context.play target_path
  end

  def download(context)
    @md5 = Digest::MD5.hexdigest @url
    target_path = context.sound_path_for @md5
    download_url_to target_path unless File.exists? target_path
    target_path
  end

  private

  def download_url_to(target_path)
    download_url_to_temporary_location do |file|
      convert_to_wav file if @url.end_with? '.mp3'
      convert_to_8000_hz_gsm file, target_path
    end
  end

  def download_url_to_temporary_location
    tmp_file = File.new "#{Rails.root}/tmp/#{@md5}#{@url.dot_extension}", "wb"

    http = EventMachine::HttpRequest.new(@url).get
    http.stream{|chunk| tmp_file.print chunk}

    f = Fiber.current
    http.callback do
      tmp_file.flush

      yield tmp_file.path

      File.delete tmp_file
      f.resume
    end
    Fiber.yield
  end

  def convert_to_wav(file)
    `lame --decode #{file} #{file}.wav`
    File.delete file
    FileUtils.mv "#{file}.wav", file
  end

  def convert_to_8000_hz_gsm(input, output)
    `sox #{input} -r 8000 -c1 #{output}`
  end
end
