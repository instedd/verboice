class PlayCommand
  def initialize(url)
    @url = url
  end

  def run(context)
    md5 = Digest::MD5.hexdigest @url

    target_filename = context.sound_path_for md5
    if File.exists? target_filename
      context.play target_filename
      return
    end

    tmp_filename = "#{Rails.root}/tmp/#{md5}"
    tmp_file = File.new tmp_filename, "wb"

    http = EventMachine::HttpRequest.new(@url).get
    http.stream do |chunk|
      tmp_file.print chunk
    end

    f = Fiber.current
    http.callback do
      `sox #{tmp_filename} -r 8000 -c1 #{target_filename}`
      File.delete tmp_file

      f.resume
    end

    Fiber.yield

    context.play target_filename
  end
end
