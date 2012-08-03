class RecordLocalizedResource < LocalizedResource

  def audio
    self.recorded_audio
  end

  def audio= an_audio_stream
    self.recorded_audio= an_audio_stream
  end

end
