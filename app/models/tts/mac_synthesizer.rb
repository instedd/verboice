class TTS::MacSynthesizer < TTS::SystemSynthesizer
  def command_for(wav_file)
    "say -o #{wav_file}"
  end

  def is_available?
    `which say` && $?.success?
  end

  def voices
  	@voices ||= load_voices
  end

  private

  def load_voices
    say_output = `say -v ?`
  	voices = Hash.new { |h, k| h[k] = [] }
  	say_output.lines.each do |line|
  	  if line =~ /(\w+(\s\w+)*)\s+(\w{2})_\w{2}/
  	    voices[$3] << {id: $1, description: $1}
  	  end
  	end
    voices
  end
end