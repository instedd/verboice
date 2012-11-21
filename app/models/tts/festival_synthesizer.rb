class TTS::FestivalSynthesizer < TTS::SystemSynthesizer
  def command_for(wav_file)
    "text2wave -o #{wav_file}"
  end

  def is_available?
    `which text2wave` && $?.success?
  end
end