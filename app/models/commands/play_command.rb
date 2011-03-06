class PlayCommand
  def initialize(url)
    @url = url
  end

  def run(context)
    # Create a hash for the url to store it locally with that name
    @md5 = Digest::MD5.hexdigest @url
    @target_filename = context.sound_path_for @md5

    # Download the sound if it does not exist locally already
    download unless File.exists? @target_filename

    # And play it!
    context.play @target_filename
  end

  private

  def download
    # Download the file to a temporary location
    dot_extension = @url.dot_extension
    tmp_file = File.new "#{Rails.root}/tmp/#{@md5}#{dot_extension}", "wb"

    http = EventMachine::HttpRequest.new(@url).get
    http.stream{|chunk| tmp_file.print chunk}

    # When done, convert it to gsm and delete the temp file
    f = Fiber.current
    http.callback do
      tmp_file.flush

      convert_to_wav tmp_file if dot_extension == '.mp3'
      convert_to_8000hz_gsm tmp_file

      File.delete tmp_file

      f.resume
    end

    Fiber.yield
  end

  def convert_to_wav(tmp_file)
    `lame --decode #{tmp_file.path} #{tmp_file.path}.wav`
    File.delete tmp_file.path
    FileUtils.mv "#{tmp_file.path}.wav", tmp_file.path
  end

  def convert_to_8000hz_gsm(tmp_file)
    `sox #{tmp_file.path} -r 8000 -c1 #{@target_filename}`
  end
end
