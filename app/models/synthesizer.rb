class Synthesizer
  include AudioUtils
  
  def initialize(pbx)
    @pbx = pbx
    @config = Rails.configuration.verboice_configuration
  end
  
  def synth(text)
    @file_id = Digest::MD5.hexdigest text
    target_path = @pbx.sound_path_for @file_id
    unless File.exists? target_path
      do_synth(text, target_path)
    end
    target_path
  end
  
  private
  
  def do_synth(text, target_path)
    if @config[:tts] == 'ispeech'
      ispeech(text, target_path)
    else
      synth_file(text, target_path)
    end
  end
  
  def ispeech(text, target_path)
    @url = "http://api.ispeech.org/api/rest?apikey=#{@config[:ispeech_api_key]}&action=convert&voice=#{@config[:ispeech_voice]}&format=#{@config[:ispeech_format]}&text=#{CGI.escape text}"
    download_url_to target_path
    target_path
  end

  def synth_file(text, target_path)
    wav_file = "#{target_path}.wave"

    if is_available? command_name
      say = IO.popen("#{command_name} -o #{wav_file}", 'w')
      say.write text
      say.close
      convert_to_8000_hz_gsm wav_file, target_path
    else
      raise "No available TTS engine. Can't execute the console command: #{command_name}"
    end
  ensure
    File.delete wav_file rescue nil
  end

  def is_available?(cmd)
    `which #{cmd}`
    return $?.success?
  end

  if RUBY_PLATFORM =~ /linux/
    def command_name
      'text2wave'
    end
  else
    def command_name
      'say'
    end
  end
end