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

  def wait_call(call_log)
    60.times do
      sleep 0.5
      call_log.reload
      break if call_log.state == :completed || call_log.state == :failed
    end

    call_log.state.should eq(:completed)
  end

  class TestSession
    def initialize(socket)
      @socket = socket
    end

    def answer
      @socket.puts "ANSWER"
      @socket.gets
    end

    def send_dtmf(digits)
      @socket.puts "EXEC SendDTMF #{digits}"
      @socket.gets
    end

    def wait_for_digit(timeout = 20000)
      @socket.puts "WAIT FOR DIGIT #{timeout}"
      @socket.gets
    end
  end

  def asterisk_session
    server = TCPServer.new("0.0.0.0", 19001)
    begin
      socket = Timeout::timeout(10) { server.accept }
      loop do
        line = socket.gets.strip
        break if line.empty?
      end

      session = TestSession.new(socket)
      session.answer

      yield session

      socket.close
    ensure
      server.close
    end
  end

end
