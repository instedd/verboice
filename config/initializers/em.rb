Thread.new { EM.run }

EM.error_handler do |err|
  p err
end

module EventMachine
  def self.fiber_sleep(time)
    fiber = Fiber.current
    add_timer(time) { fiber.resume }
    Fiber.yield
  end
end
