module Voxeo
  class FiberStore
    include Singleton
    
    TIMEOUT = 5 * 60
    
    def initialize
      @fibers = {}
      @timers = {}
    end
    
    def store_fiber(key, fiber)
      @fibers[key] = fiber
      renew_em_timer key, fiber
      fiber
    end
    
    def get_fiber_for(key)
      fiber = @fibers[key]
      renew_em_timer(key, fiber) if fiber
      fiber
    end
    
    def delete_fiber_for(key)
      fiber = @fibers.delete key
      timer = @timers.delete key
      EM.cancel_timer(timer) if timer
      fiber
    end
    
    private
    
    def renew_em_timer(key, fiber)
      current_timer = @timers[key]
      EM.cancel_timer(current_timer) if current_timer
      
      @timers[key] = EM.add_timer TIMEOUT do 
        @fibers.delete key
        @timers.delete key
        fiber.resume Exception.new("Session timeout")
      end
    end
    
  end
end