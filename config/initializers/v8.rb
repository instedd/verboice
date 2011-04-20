class V8::Object
  def to_hash
    hash = {}
    each { |key, value| hash[key] = value }
    hash
  end

  def respond_to?(method)
    method == :to_hash || super
  end
end
