module AsteriskCall

  def make_call(call_flow, channel, address)
    channel.call address, call_flow_id: call_flow.id
  end

  def make_interactive_call(call_flow, channel, address, &block)
    call_log = make_call(call_flow, channel, address)
    asterisk_session(&block)
    call_log.reload
    call_log
  end

  def make_remote_interactive_call(&block)
    with_server do |server|
      asterisk_ami(:remote) do |ami|
        originate_call(ami)
        asterisk_session(server) do |session|
          block.call(session)
        end
      end
    end
    sleep 0.5
    CallLog.last
  end

  def originate_call(ami)
    originate = ami.originate("PJSIP/1234567890@verboice", "verboice", "1234567890", "1", "")
    Rails.logger.info "Originate call response:\n#{originate.raw_response}"
    originate.success.should be_true, "Expected originate call to be successful, but was:\n#{originate.inspect}"
  end

  def asterisk_ami(target, &block)
    port = case target
    when :remote then 5039
    when :verboice then 5038
    else raise "Invalid asterisk in opening AMI"
    end

    begin
      ami = RubyAsterisk::AMI.new('127.0.0.1', port)
      ami.login('verboice', 'verboice').success.should be_true
      block.call(ami)
    ensure
      ami.try(&:disconnect)
    end
  end

  def wait_call(call_log)
    60.times do
      sleep 0.5
      call_log.reload
      break if call_log.state == :completed || call_log.state == :failed
    end

    call_log.state.should eq(:completed)
  end

  def wait_for_channel_definition(channel_id)
    Rails.logger.info("Waiting for channel #{channel_id} definition on verboice asterisk")
    asterisk_ami(:verboice) do |ami|
      15.times do
        response = ami.command("pjsip show endpoint verboice_#{channel_id}").raw_response
        unless response =~ /Unable to find object/
          Rails.logger.info("Channel #{channel_id} successfully defined on verboice asterisk")
          return
        end
        # ami.command("pjsip reload")
        sleep 2
      end
    end
    raise "Channel #{channel_id} not defined on verboice asterisk"
  end

  def asterisk_remote_file(name)
    File.join(Rails.root, "spec/support/etc/asterisk-remote", name)
  end

  def run_erb(asterisk, name, bind)
    erb = File.join(Rails.root, "spec/support/etc/asterisk-#{asterisk.to_s}", "#{name}.erb")
    target = File.join(Rails.root, "spec/support/etc/asterisk-#{asterisk.to_s}", name)
    File.write(target, ERB.new(File.read(erb)).result(bind))
  end

  def config_remote_asterisk(host_ip, channel_id)
    Rails.logger.info("Configuring remote asterisk with AGI extension to IP #{host_ip} and PJSIP registration for #{channel_id}")
    run_erb(:remote, "pjsip_register.conf", binding)
    run_erb(:remote, "extensions.conf", binding)
    asterisk_ami(:remote) do |ami|
      ami.command("dialplan reload")
      sleep 1
      ami.command("pjsip reload")
      wait_for_channel_registration(ami)
    end
  end

  def config_verboice_asterisk(host_ip)
    Rails.logger.info("Configuring verboice asterisk with AGI extension to IP #{host_ip}")
    run_erb(:verboice, "extensions.conf", binding)
    asterisk_ami(:verboice) do |ami|
      ami.command("dialplan reload")
      sleep 1
    end
  end

  def unconfig_remote_asterisk
    File.delete(asterisk_remote_file("pjsip_register.conf")) rescue nil
  end

  def wait_for_channel_registration(ami)
    Rails.logger.info("Waiting for channel registration on remote asterisk")
    status = nil
    30.times do
      response = ami.command('pjsip show registration verboice').raw_response
      response =~ /verboice\/sip:172.28.128.10\s+verboice\s+(\w+)/
      status = $1
      if status == "Registered"
        Rails.logger.info("Channel registered in remote asterisk")
        return
      end
      Rails.logger.debug("Response: #{status}")
      sleep 1
    end
    status.should eq("Registered")
  end

  def assert_dtmf(file, digit_or_digits)
    source_path = File.join(Rails.root, "spec/support/etc/", file)
    file_path = File.join(Rails.root, "spec/support/etc/", file.gsub(/\.\w+$/, "-processed.au"))
    detector_path = File.join(Rails.root, "spec/support/bin/detect-dtmf-au")
    Rails.logger.info `ffmpeg -y -i #{source_path} -map_metadata 0 -ar 8000 -acodec pcm_s8 #{file_path} 2>&1`
    detection = `#{detector_path} #{file_path} 2>&1`
    found = detection.scan(/`(\d)'/).flatten.uniq
    expected = Array.wrap(digit_or_digits).map(&:to_s)
    found.should eq(expected)
  end

  class TestSession
    def initialize(socket)
      @socket = socket
    end

    def answer
      @socket.puts "ANSWER"
      Rails.logger.debug "ANSWER: #{@socket.gets}"
    end

    def send_dtmf(digits)
      @socket.puts "EXEC SendDTMF #{digits}"
      Rails.logger.debug "SendDTMF: #{@socket.gets}"
    end

    def wait_for_digit(timeout = 20000)
      @socket.puts "WAIT FOR DIGIT 20000"
      Rails.logger.debug "WAIT FOR DIGIT: #{@socket.gets}"
    end

    def record_call(file, timeout = 20000)
      @socket.puts "RECORD FILE \"#{file}\" au \"#\" 10000"
      Rails.logger.debug "RECORDING: #{@socket.gets}"
    end

    def play_file(file)
      @socket.puts "STREAM FILE \"#{file}\" \"\""
      Rails.logger.debug "STREAMING FILE: #{@socket.gets}"
    end

    def hangup
      @socket.puts "HANGUP"
      Rails.logger.debug "HANGUP: #{@socket.gets}"
    end
  end

  def with_server
    server = TCPServer.new("0.0.0.0", 19001)
    begin
      yield server
    ensure
      server.try(:close)
    end
  end

  def asterisk_session(server)
    socket = Timeout::timeout(10) { server.accept }
    loop do
      line = socket.gets.strip
      break if line.empty?
    end

    session = TestSession.new(socket)
    session.answer

    yield session

    socket.close
  end

end
