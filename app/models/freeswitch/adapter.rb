module Freeswitch
  class Adapter
    InstallDir = '/usr/local/freeswitch'
    SoundsPath = "#{InstallDir}/sounds/verboice/"

    def initialize(context)
      FileUtils.mkdir_p SoundsPath
      @context = context
      @synthesizer = Synthesizer.new self
    end

    def channel_id; @context.session[:variable_verboice_channel_id]; end
    def call_log_id; @context.session[:variable_verboice_call_log_id]; end
    def caller_id; @context.session[:variable_effective_caller_id_number]; end

    def answer
      @context.answer
    end

    def hangup
      @context.hangup
    end

    def sound_path_for(basename)
      "#{SoundsPath}#{basename}.gsm"
    end
    
    def say(text)
      filename = @synthesizer.synth text
      play filename
    end

    def play(filename)
      @context.playback filename
    end

    def capture(options)
      options[:play] = @synthesizer.synth(options[:say]) if options[:say]
      file = options[:play] || 'silence_stream://1'
      freeswitch_options = {
        :min => options[:min],
        :max => options[:max],
        :terminators => options[:finish_on_key],
        :timeout => options[:timeout] * 1000,
        :variable => 'capture'
      }
      @context.read file, freeswitch_options
    end

    def record
      @context.record "#{SoundsPath}/foo.wav"
    end
    
    def pause(length)
      EM.fiber_sleep length
    end

    def is_answering_machine?
      false
    end
  end
end
