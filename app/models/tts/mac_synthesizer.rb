class TTS::MacSynthesizer < TTS::SystemSynthesizer
  def command_for(wav_file)
    "say -o #{wav_file}"
  end

  def is_available?
    `which say` && $?.success?
  end
end