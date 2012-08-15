class RecordLocalizedResource < LocalizedResource

  def audio
    self.recorded_audio
  end

  def audio= an_audio_stream
    self.recorded_audio= an_audio_stream
  end

  def play_command_for play_resource_command
    play_resource_command.play_record_command_for self
  end

  def capture_resource_for play_resource_command, session
    play_resource_command.record_capture_resource_for self, session
  end
end
