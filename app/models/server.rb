class Server < Struct.new(:host, :register, :direction)
  def register?
    register == '1'
  end
end
