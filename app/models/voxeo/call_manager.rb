module Voxeo
  class CallManager

    def initialize session_id
      p "Creating call manager with session id #{session_id}"
      @session_id = session_id
      @builder = Builders::Vxml.new
      @hangup = false
    end

    def session_id
      @session_id
    end

    def channel_id
      Channel.first.id
    end

    def caller_id
      @params && @params['session.callerid']
    end

    def answer
      p "Call manager received answer command"
    end

    def play(filename, escape_digits = nil)
      p "Call manager received play command"
    end

    def hangup
      p "Call manager received hangup command"
      return if @hangup

      @builder.say ("Hello World " * 20)

      # Set hangup to true, defer the operation to resume the fiber so the session can end
      @hangup = true
      EM.defer { Fiber.current.resume }

      flush
    end

    def capture(options)
    end

    def is_answering_machine?
      false
    end

    def sound_path_for(basename)
      Rails.root.join "public", "sounds", basename
    end

    def flush
      @params = Fiber.yield @builder.build
    end

  end
end