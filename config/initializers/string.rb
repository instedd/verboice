class String

  def is_integer?
    return true if Integer(self) rescue false
  end

  def is_number?
    is_integer? || (true if Float(self) rescue false)
  end

  def try_as_number
    if is_integer?
      Integer(self)
    elsif is_number?
      Float(self)
    else
      self
    end
  end

end
