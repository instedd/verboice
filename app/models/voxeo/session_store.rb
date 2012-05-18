module Voxeo

  class SessionStore
    include Singleton

    Timeout = 5 * 60

    def initialize
      @sessions = {}
      @timers = {}
    end

    def session_for(key)
      renew_em_timer(key)
      @sessions[key] ||= Voxeo::Session.new
    end

    private

    def renew_em_timer(key)
      current_timer = @timers[key]
      EM.cancel_timer(current_timer) if current_timer

      @timers[key] = EM.add_timer Timeout do
        @timers.delete key
        fiber = @sessions.delete(key).try(:get_fiber)
        fiber.resume(Exception.new("Session timeout")) if fiber
      end
    end

  end

  class Session

    def initialize
      @session = {}
      @fiber = nil
    end

    def store_fiber(fiber)
      @fiber = fiber
    end

    def get_fiber
      @fiber
    end

    def end!
      @fiber = nil
    end

    def store(key, value)
      @session[key] = value
    end

    def get(key)
      @session[key]
    end

  end

end