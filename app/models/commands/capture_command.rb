class CaptureCommand
  def initialize(options = {})
    @options = {:min => 1, :max => 1, :finish_on_key => '#', :timeout => 5}
    @options.merge! options
    @options[:max] = Float::INFINITY if @options[:max] < @options[:min]
  end

  def run(session)
    @options[:play] = PlayCommand.new(@options[:play]).download(session) if @options[:play]

    digits = session.pbx.capture @options
    session[:last_capture] = digits
  end
end
