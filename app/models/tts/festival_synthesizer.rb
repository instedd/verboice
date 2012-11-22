class TTS::FestivalSynthesizer < TTS::SystemSynthesizer
  def command_for(voice, wav_file)
    "text2wave -o #{wav_file}"
  end

  def is_available?
    `which text2wave` && $?.success?
  end

  def voices
    @voices ||= load_voices
  end

  private

  def load_voices
    voices = Hash.new { |h, k| h[k] = [] }
    IO.popen 'festival --pipe', 'r+' do |festival|
      festival.write File.read(File.expand_path('../list_festival_voices.scm', __FILE__))
      festival.close_write
      festival.readlines.each do |line|
        voice, language, gender, dialect = line.chomp!.split('|')
        lang_code = LanguageList::LanguageInfo.find_by_name(language.titleize).iso_639_1
        voices[lang_code] << {id: voice, description: "#{dialect.titleize} #{language.titleize} #{gender.titleize} (#{voice})"}
      end
    end
    voices
  end
end