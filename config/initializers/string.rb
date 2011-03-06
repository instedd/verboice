class String
  def dot_extension
    self[/\.[^\.]+$/] || ''
  end
end
