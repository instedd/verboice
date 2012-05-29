class Object
  def not_nil?
    !nil?
  end

  def is_an? object
    is_a? object
  end

  def deep_clone
    Marshal.load( Marshal.dump( self ) )
  end

end