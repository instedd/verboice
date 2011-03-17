require 'fiber'

class Fiber
  class << self
    def yield_with_exception
      result = yield_without_exception
      raise result if result.is_a? Exception
      result
    end
    alias_method_chain :yield, :exception
  end

  def resume_with_exception(*args)
    if args.length == 1 && args[0].is_a?(Exception) && self == Fiber.current
      raise args[0]
    end
    resume_without_exception *args
  end
  alias_method_chain :resume, :exception
end
