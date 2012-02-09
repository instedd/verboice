module Asterisk
  class FastAGIProtocol < EventMachine::Protocols::LineAndTextProtocol
    include Asterisk::AGIMixin
    undef exec

    def initialize
      super
      @agi_mode = :environment
      @agi_queue = []
      @agi_last_defer = nil
    end

    def receive_line(line)
      if @agi_mode == :environment
        if not parse_env line
          @agi_mode = :commands
          f = Fiber.new do
            agi_post_init
          end
          f.resume
        end
      else # @agi_mode == :commands
        @log.debug "<< "+line if not @log.nil?
        if line == "HANGUP"
          @agi_last_defer.succeed Exception.new 'User hung up' if @agi_last_defer
          @hangup = true
        else
          return if @agi_last_defer.nil?
          @agi_last_defer.succeed Response.new(line)
          @agi_last_defer = nil
          flush_queue
        end
      end
    rescue Exception => e
      if not @log.nil?
        @log.error "#{e.class.name}: #{e.message}"
        e.backtrace.each { |line| @log.error "\t#{line}" }
      end
    end

    def unbind
      @agi_last_defer.succeed Exception.new 'Communication broken' if @agi_last_defer
      @hangup = true
      super
    end

    # Send a command, and return a Deferrable for the Response object.
    def send_command(cmd, *args)
      if @hangup
        raise Exception.new 'Communication broken'
      end

      f = Fiber.current
      msg = build_msg(cmd, *args)
      d = EM::DefaultDeferrable.new
      @agi_queue << [msg, d]
      flush_queue
      d.callback do |line|
        f.resume line
      end
      Fiber.yield
    end

    # Try to send the next command from the queue.
    def flush_queue
      return if not @agi_last_defer.nil?
      msg, d = @agi_queue.shift
      return if msg.nil?
      @agi_last_defer = d
      @log.debug ">> "+msg if not @log.nil?
      send_data msg+"\n"
    end
    private :flush_queue
  end
end
