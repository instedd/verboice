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
end