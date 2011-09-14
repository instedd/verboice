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
    wav_file = "#{get_target_path(session)}.wave"

    if is_available? 'say'
      say = IO.popen("say -o #{wav_file}", 'w')
      say.write @text
      say.close
      convert_to_8000_hz_gsm wav_file, get_target_path(session)
    else
      raise "No available TTS engine"
    end
  ensure
    File.delete wav_file rescue nil
  end

  def is_available?(cmd)
    `which #{cmd}`
    return $?.success?
  end
end
