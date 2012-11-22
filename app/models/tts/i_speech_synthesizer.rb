class TTS::ISpeechSynthesizer < TTS::Synthesizer
  def synth(text, voice, target_path)
    url = "http://api.ispeech.org/api/rest?apikey=#{@config[:ispeech_api_key]}&action=convert&voice=#{voice}&format=#{@config[:ispeech_format]}&text=#{CGI.escape text}"
    download_url_to url, target_path
  end

  def voices
  	@voices ||= load_voices
  end

  private

  def load_voices
  	ispeech_voices = JSON.parse RestClient.get('http://api.ispeech.org/api/rest?apikey=developerdemokeydeveloperdemokey&action=information&output=json')
    tmp_voices = Hash.new { |h, k| h[k] = {} }
  	ispeech_voices.each do |key, value|
      if key =~ /voice-(\d+)/
        tmp_voices[$1][:id] = value
      elsif key =~ /voice-locale-(\d+)/
        tmp_voices[$1][:locale] = value[0..1]
      elsif key =~ /voice-description-(\d+)/
        tmp_voices[$1][:description] = value
      end
  	end


    voices = Hash.new { |h, k| h[k] = [] }
    tmp_voices.values.each do |voice|
      voices[voice[:locale]] << voice.except(:locale)
    end

    voices
  end
end