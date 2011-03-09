class CaptureCommand
  def initialize(options = {})
    @options = {:min => 1, :max => 1, :finish_on_key => '#', :timeout => 5}
    @options.merge! options
    @options[:max] = Float::INFINITY if @options[:max] < @options[:min]
  end

  def run(context)
    @options[:play] = PlayCommand.new(@options[:play]).download(context) if @options[:play]

    digits = context.capture @options
    context.set_last_capture digits
  end
end
