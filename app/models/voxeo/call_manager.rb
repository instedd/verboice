module Voxeo
  class CallManager

    attr_reader :session_id, :voxeo_session_id, :channel_id, :caller_id

    def initialize channel_id, voxeo_session_id, opts = {}
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
      # begin
        @context = Fiber.yield @builder.build
      # rescue Exception => e
      #   handle_error e
      # end
    end

    def handle_error(e)
      @hangup = true

      @builder.say "An unexpected error ocurred"

      # End the session from the store
      Voxeo::SessionStore.instance.session_for(@voxeo_session_id).end!

      # Enqueue operation to resume the fiber so the session can end
      current_fiber = Fiber.current
      EM.next_tick { current_fiber.resume e }

      Fiber.yield @builder.build
    end

    def end_session
      @hangup = true

      # End the session from the store
      Voxeo::SessionStore.instance.session_for(@voxeo_session_id).end!

      # Enqueue operation to resume the fiber so the session can end
      current_fiber = Fiber.current
      EM.next_tick { current_fiber.resume }

      flush
    end

    def sounds_path
      File.join(Rails.root, 'data', 'voxeo')
    end

    def callback_url
      Voxeo::UrlHelper.callback_url :host => @context.headers[:Host]
    end

    def sounds_url_for(filename)
      key = Guid.new.to_s
      Voxeo::SessionStore.instance.session_for(@voxeo_session_id).store(key, filename)
      Voxeo::UrlHelper.audio_url key, :sessionid => @voxeo_session_id, :host => @context.headers[:Host]
    end

  end
end