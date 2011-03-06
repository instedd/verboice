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
    http = EventMachine::HttpRequest.new(@url).get

    tmp_file = File.new "#{Rails.root}/tmp/#{@md5}", "wb"
    http.stream{|chunk| tmp_file.print chunk}

    # When done, convert it to gsm and delete the temp file
    f = Fiber.current
    http.callback do
      tmp_file.flush
      `sox #{tmp_file.path} -r 8000 -c1 #{@target_filename}`
      File.delete tmp_file

      f.resume
    end

    Fiber.yield
  end
end
