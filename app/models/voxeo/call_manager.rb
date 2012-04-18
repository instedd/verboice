module Voxeo
  class CallManager
    
    attr_reader :session_id, :voxeo_session_id, :channel_id, :caller_id

    def initialize channel_id, voxeo_session_id, session_id = nil, caller_id = nil
      @channel_id = channel_id
      @session_id = session_id
      @caller_id = caller_id
      @voxeo_session_id = voxeo_session_id
      @builder = Builders::Vxml.new
      @hangup = false
    end

    def answer
    end

    def play(filename, escape_digits = nil)
      return if @hangup
      @builder.play filename
    end
    
    def say(text)
      return if @hangup
      @builder.say text
    end
    
    def pause(length)
      return if @hangup
      @builder.pause length
    end
    
    def capture(options)
      return if @hangup
      
      @builder.capture(options)
      @builder.callback("http://staging.instedd.org:7000/?session.sessionid=#{@voxeo_session_id}")
      
      flush
      @params[:digits]
    end

    def hangup
      return if @hangup
      
      @builder.hangup
      
      end_session
    end
    
    def bridge_with(other_session)
      # TODO
    end
    
    def dial(address, options = {})
      # TODO
    end

    def is_answering_machine?
      false
    end

    def sound_path_for(basename)
      Rails.root.join "public", "sounds", "#{basename}.gsm"
    end
    
    private

    def flush
      @params = Fiber.yield @builder.build
    end
    
    def end_session
      @hangup = true
      
      # Remove the fiber from the store
      Voxeo::FiberStore.instance.delete_fiber_for @voxeo_session_id
      
      # Enqueue operation to resume the fiber so the session can end
      current_fiber = Fiber.current
      EM.next_tick { current_fiber.resume }
      
      flush
    end

  end
end