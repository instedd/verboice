class ImplicitVariable

  def initialize(contact)
    @contact = contact
  end

  def value
    subclass_responsibility
  end

  def self.key
    subclass_responsibility
  end

  def self.find(key)
    self.subclasses.detect{|s| s.key == key}
  end

end