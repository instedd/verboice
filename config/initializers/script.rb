module Kernel
  def script(&block)
    s = Script.new &block
    s.commands
  end
end
