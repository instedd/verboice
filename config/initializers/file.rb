class File
  def self.mime_type(path)
    `file --mime --brief #{path}`.strip[/^[^;]*/]
  end

  def self.is_mpeg?(path)
    self.mime_type(path) == 'audio/mpeg'
  end

  def self.is_wav?(path)
    self.mime_type(path) == 'audio/x-wav'
  end
end
