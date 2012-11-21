class TTS::ISpeechSynthesizer < TTS::Synthesizer
  def synth(text, target_path)
    url = "http://api.ispeech.org/api/rest?apikey=#{@config[:ispeech_api_key]}&action=convert&voice=#{@config[:ispeech_voice]}&format=#{@config[:ispeech_format]}&text=#{CGI.escape text}"
    download_url_to url, target_path
  end
end