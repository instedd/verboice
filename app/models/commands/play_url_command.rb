class Commands::PlayUrlCommand < Command
  include Commands::PlayCommand
  param :url, :string, :ui_length => 80

  def initialize(url)
    @url = url
    super Digest::MD5.hexdigest @url
  end

  def run(session)
    session.info "Play #{@url}"
    super
  end

  def setup_file(session)
    target_path = get_target_path(session)
    session.trace "Download #{@url}"
    download_url_to target_path
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
