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

  def capture(options)
    digits = ''
    options[:max].times do
      digit = capture_digit(options[:timeout] * 1000)
      break if digit.nil? || options[:finish_on_key].include?(digit)

      digits << digit
    end
    digits.length < options[:min] ? nil : digits
  end

  def set_last_capture(digits)
    p digits
  end

  private

  def capture_digit(timeout)
    line = @context.wait_for_digit timeout
    case line.result
    when '0' then nil
    when '35' then "#"
    when '42' then "*"
    else "#{line.result.to_i - 48}"
    end
  end
end