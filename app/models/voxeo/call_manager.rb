module Voxeo
  class CallManager
    
    attr_reader :session_id, :voxeo_session_id, :channel_id, :caller_id

    def initialize channel_id, voxeo_session_id, opts = {} #session_id = nil, caller_id = nil, context = nil
      @channel_id = channel_id
      @voxeo_session_id = voxeo_session_id
      @session_id = opts[:session_id]
      @caller_id = opts[:caller_id]
      @context = opts[:context]
      @builder = Builders::Vxml.new "sessionid" => voxeo_session_id
      @hangup = false
      @config = Rails.configuration.voxeo_configuration
    end

    def answer
    end

    def play(filename, escape_digits = nil)
      return if @hangup
      @builder.play sounds_url_for(filename)
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
      
      options[:play] = sounds_url_for(options[:play]) if options[:play]
      
      @builder.capture options
      @builder.callback callback_url
      
      flush
      @context.params[:digits]
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
      File.join sounds_path, "#{basename}.gsm"
    end
    
    private

    def flush
      @context = Fiber.yield @builder.build
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
    
    def callback_url
      host = @config[:http_url_options][:host]
      port = @config[:http_url_options][:port]
      
      if host.present? && port.present?
        "http://#{host}:#{port}/"
      else
        "http://#{@context.headers[:Host]}/"
      end
    end
    
    def sounds_url_for(filename)
      basename = filename[sounds_path.size..-1]
      "#{@config[:sounds_url]}#{basename}"
    end
    
    def sounds_path
      File.join(Rails.public_path, "sounds")
    end

  end
end