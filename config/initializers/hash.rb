class Hash
  def to_pretty_s
    reject {|k, v| v.blank?}.map{ |k, v| "#{k}: #{v}" }.join ', '
  end
end
