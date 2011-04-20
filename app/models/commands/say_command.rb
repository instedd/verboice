class SayCommand < PlayCommand
  def initialize(text)
    super "http://translate.google.com/translate_tts?tl=en&q=#{CGI.escape text}"
  end
end
