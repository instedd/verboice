module MarshalZipSerializable
  extend ActiveSupport::Concern

  module ClassMethods
    def dump(x)
      Zlib.deflate(Marshal.dump(x), 9)
    end

    def load(x)
      return nil if x.nil?
      data = nil

      # Marshal.load fails with large flows when run inside a Fiber (Fiber stacks are 4k only)
      # Run the unmarshaling in a thread so we can use a full stack
      Thread.new { data = Marshal.load(Zlib.inflate(x)) rescue nil }.join
      data
    end
  end

end