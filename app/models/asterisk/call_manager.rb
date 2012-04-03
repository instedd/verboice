class FastAgiProtocol < EventMachine::Protocols::LineAndTextProtocol; end

module Asterisk
  class CallManager < Asterisk::FastAgiProtocol
    Port = Rails.configuration.asterisk_configuration[:call_manager_port].to_i
    SoundsDir = Rails.configuration.asterisk_configuration[:sounds_dir]
    SoundsPath = "#{SoundsDir}/verboice/"
    AgiSeparator = Rails.configuration.asterisk_configuration[:agi_use_pipe_separator] == true ? '|' : ','

    def agi_post_init
      FileUtils.mkdir_p SoundsPath
      @log = Rails.logger
      BaseBroker.instance.accept_call self
    end

    def channel_id
      (self['arg_2'] || self['extension']).to_i
    end

    def session_id
      self['arg_1']
    end

    def caller_id
      self['callerid']
    end

    def hangup
      send_command 'HANGUP'
    rescue Exception => ex
      # Ignore these errors
    ensure
      close_connection
    end

    def bridge_with(other_session)
      exec 'Bridge', other_session.pbx['channel']
    end

    def dial(address, options = {})
      set_callerid options[:caller_id] if options[:caller_id]
      exec 'Dial', [address, 30, "m"].join(AgiSeparator)
      status = get_variable 'DIALSTATUS'
      case status.note
      when 'ANSWER' then :completed
      when 'BUSY' then :busy
      when 'NOANSWER' then :no_answer
      when 'CANCEL' then raise Exception.new 'User hang up'
      else :failed
      end
    end

    def sound_path_for(basename)
      "#{SoundsPath}#{basename}.gsm"
    end

    def play(filename, escape_digits = nil)
      filename = filename[SoundsPath.length .. -5] # Remove SoundsPath and .gsm extension
      line = stream_file("verboice/#{filename}", escape_digits)
      if line.result == '-1'
        raise Exception.new 'Error while playing file'
      end
      ascii_to_number line.result
    end

    def capture(options)
      [:min, :max, :timeout].each { |key| options[key] = options[key].to_i rescue nil }

      digits = ''

      if options[:play]
        play_digit = play(options[:play], '0123456789#*')
        if play_digit
          return :finish_key if options[:finish_on_key].include? play_digit

          digits << play_digit
        end
      end

      until digits.length == options[:max]
        digit = capture_digit(options[:timeout] * 1000)

        # Timeout
        if digit.nil?
          return :timeout if digits.length < options[:min]
          break
        end

        # Terminator
        if options[:finish_on_key].include? digit
          if digits.length == 0
            return :finish_key
          else
            break
          end
        end

        digits << digit
      end
      digits.length < options[:min] ? nil : digits
    end

    def record
      record_file "#{SoundsPath}/foo", 'wav', '0123456789*#', '5000', 'beep'
    end

    def is_answering_machine?
      # TODO: add configuration for this. For now it's kind of annoying when it doesn't work.
      return false

      amd_result = exec('amd').result
      return false if amd_result.to_i == -2

      get_variable('amdstatus').raw =~ /MACHINE/
    end

    private

    def capture_digit(timeout)
      line = wait_for_digit timeout
      ascii_to_number line.result
    end

    def ascii_to_number(ascii)
      case ascii
      when '0' then nil
      when '35' then "#"
      when '42' then "*"
      else "#{ascii.to_i - 48}"
      end
    end
  end
end
