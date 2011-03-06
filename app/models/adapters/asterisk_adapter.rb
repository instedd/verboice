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

  def install_sound(filename)
    target = sound_path_for File.basename(filename)
    `sox #{filename} -r 8000 -c1 #{target}`
    target
  end

  def play(filename)
    filename = filename[SoundsPath.length .. -5] # Remove sounds_path and .gsm extension
    @context.stream_file("verbo/#{filename}", nil)
  end
end
