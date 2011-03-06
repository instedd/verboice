class AsteriskAdapter
  InstallDir = '/usr/local/asterisk'
  SoundsPath = "#{InstallDir}/var/lib/asterisk/sounds/verbo/"
  FileUtils.mkdir_p SoundsPath

  def initialize(context)
    @context = context
  end

  def answer
    @context.answer
  end

  def hangup
    @context.hangup
    @context.close_connection
  end

  def sound_path_for(basename)
    "#{SoundsPath}#{basename}.gsm"
  end

  def play(filename)
    filename = filename[SoundsPath.length .. -5] # Remove SoundsPath and .gsm extension
    @context.stream_file("verbo/#{filename}", nil)
  end
end
