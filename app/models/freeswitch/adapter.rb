module Freeswitch
  class Adapter
    InstallDir = '/usr/local/freeswitch'
    SoundsPath = "#{InstallDir}/sounds/verboice/"
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

    def capture(options)
      file = options[:play] || 'silence_stream://1'
      freeswitch_options = {
        :min => options[:min],
        :max => options[:max],
        :terminators => options[:finish_on_key],
        :timeout => options[:timeout] * 1000,
        :variable => 'last_capture'
      }
      @context.read file, freeswitch_options
    end

    def record
      @context.record "#{SoundsPath}/foo.wav"
    end
  end
end
