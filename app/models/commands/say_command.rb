class SayCommand < Command
  include PlayCommand
  param :text, :string, :ui_length => 80

  def initialize(text)
    @text = text
    super Digest::MD5.hexdigest text
  end

  def run(session)
    session.info "Say '#{@text}'"
    super
  end

  def setup_file(session)
    @config = Rails.configuration.verboice_configuration
    if config[:tts] == 'ispeech'
      ispeech session
    else
      synth_file session
    end
  end

  def ispeech(session)
    @url = "http://api.ispeech.org/api/rest?apikey=#{@config[:ispeech_api_key]}&action=convert&voice=#{@config[:ispeech_voice]}&text=#{CGI.escape @text}"
    target_path = get_target_path(session)
    session.trace "Generating voice file with iSpeech"
    download_url_to target_path
    target_path
  end

  def synth_file(session)
    wav_file = "#{get_target_path(session)}.wave"

    if is_available? command_name
      say = IO.popen("#{command_name} -o #{wav_file}", 'w')
      say.write @text
      say.close
      convert_to_8000_hz_gsm wav_file, get_target_path(session)
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
