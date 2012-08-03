class UploadLocalizedResource < LocalizedResource

  def audio
    self.uploaded_audio
  end

  def audio= an_audio_stream
    self.uploaded_audio= an_audio_stream
  end

end
