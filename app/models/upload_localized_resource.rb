class UploadLocalizedResource < LocalizedResource

  def audio
    self.uploaded_audio
  end

  def audio= an_audio_stream
    self.uploaded_audio= an_audio_stream
  end

  def play_command_for play_resource_command
    play_resource_command.play_upload_command_for self
  end

  def capture_resource_for play_resource_command, session
    play_resource_command.upload_capture_resource_for self, session
  end
end
