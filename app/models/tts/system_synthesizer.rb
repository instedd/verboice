class TTS::SystemSynthesizer < TTS::Synthesizer
  if RUBY_PLATFORM =~ /darwin/
    def self.instance
      TTS::MacSynthesizer.new
    end
  else
    def self.instance
      TTS::FestivalSynthesizer.new
    end
  end

  def synth(text, target_path)
    wav_file = "#{target_path}.wave"

    if is_available?
      say = IO.popen command_for(wav_file), 'w'
      say.write text
      say.close
      convert_to_8000_hz_gsm wav_file, target_path
    else
      raise "No available TTS engine. Can't execute the console command: #{command_name}"
    end
  ensure
    File.delete wav_file rescue nil
  end
end