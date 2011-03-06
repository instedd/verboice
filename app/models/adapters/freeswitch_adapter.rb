class FreeswitchAdapter
  InstallDir = '/usr/local/freeswitch'
  SoundsPath = "#{InstallDir}/sounds/verbo/"
  FileUtils.mkdir_p SoundsPath

  def initialize(context)
    @context = context
  end

  def answer
    @context.answer
  end

  def hangup
    @context.hangup
  end

  def sound_path_for(basename)
    "#{SoundsPath}#{basename}.gsm"
  end

  def play(filename)
    @context.playback filename
  end
end
