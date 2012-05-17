module MarshalZipSerializable
  extend ActiveSupport::Concern

  module ClassMethods
    def dump(x)
      Zlib.deflate(Marshal.dump(x), 9)
    end

    def load(x)
      return nil if x.nil?
      Marshal.load(Zlib.inflate(x)) rescue nil
    end
  end

end