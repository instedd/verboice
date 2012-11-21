# Copyright (C) 2010-2012, InSTEDD
#
# This file is part of Verboice.
#
# Verboice is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# Verboice is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with Verboice.  If not, see <http://www.gnu.org/licenses/>.

module Asterisk
  class CallManager < Batphone::FiberedFastAgiProtocol
    undef exec
    Port = Rails.configuration.asterisk_configuration[:call_manager_port].to_i
    SoundsDir = Rails.configuration.asterisk_configuration[:sounds_dir]
    SoundsPath = "#{SoundsDir}/verboice/"
    AgiSeparator = Rails.configuration.asterisk_configuration[:agi_use_pipe_separator] == true ? '|' : ','

    attr_accessor :session

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

    def say(text, options = {})
      filename = session.synthesizer.synth text
      play filename, options
    end

    def sound_path_for(basename)
      "#{SoundsPath}#{basename}.gsm"
    end

    def play(filename, options = {}, escape_digits = nil)
      # Usage: STREAM FILE <filename> <escape digits> [sample offset]
      # Send the given file, allowing playback to be interrupted by the given digits, if any.
      # Use double quotes for the digits if you wish none to be permitted.
      # If sample offset is provided then the audio will seek to sample offset before play starts.
      # Remember, the file extension must not be included in the filename.
      #
      # failure: 200 result=-1 endpos=<sample offset>
      # failure on open: 200 result=0 endpos=0
      # success: 200 result=0 endpos=<offset>
      # digit pressed: 200 result=<digit> endpos=<offset>
      #
      # <offset> is the stream position streaming stopped. If it equals <sample offset> there was probably an error.
      # <digit> is the ascii code for the digit pressed.

      filename = filename[SoundsPath.length .. -5] # Remove SoundsPath and .gsm extension
      line = stream_file("verboice/#{filename}", escape_digits)
      if line.result == '-1'
        options[:if_hang_up].call line.endpos if options[:if_hang_up].present?
        raise Exception.new 'User hung up'
      end
      if line.result == '0' && line.endpos == 0
        raise Exception.new 'Error while playing file'
      end
      [ascii_to_number(line.result), line.endpos]
    end

    def capture(options)
      [:min, :max, :timeout].each { |key| options[key] = options[key].to_i rescue nil }
      options[:play] = session.synthesizer.synth(options[:say]) if options[:say]

      digits = ''

      if options[:play]
        play_digit, offset = play(options[:play], { if_hang_up: options[:if_hang_up] }, '0123456789#*')
        options[:after_play].call play_digit, offset if options[:after_play].present?
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

    def record filename, stop_keys, timeout
      # Ensure file exists and asterisk has write permissions
      FileUtils.touch(filename) unless File.exists?(filename)
      File.chmod(0666, filename)

      timeout = (timeout * 1000).to_s
      ext = File.extname(filename).sub('.','')
      filename_without_ext = filename.chomp(File.extname(filename))

      record_file filename_without_ext, ext, stop_keys, timeout, 'beep'

      # Set permissions back to default
      File.chmod(0644, filename)
    end

    def is_answering_machine?
      # TODO: add configuration for this. For now it's kind of annoying when it doesn't work.
      return false

      amd_result = exec('amd').result
      return false if amd_result.to_i == -2

      get_variable('amdstatus').raw =~ /MACHINE/
    end

    def pause(length)
      EM.fiber_sleep length
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
