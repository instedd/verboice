class CaptureCommand < Command
  param :min, :integer, :default => 1, :ui_length => 1
  param :max, :integer, :default => 1, :ui_length => 1
  param :finish_on_key, :string, :default => '#', :ui_length => 1
  param :timeout, :integer, :default => 5, :ui_length => 1
  param :play, :string, :ui_length => 80

  def initialize(options = {})
    @options = {:min => 1, :max => 1, :finish_on_key => '#', :timeout => 5}
    @options.merge! options.symbolize_keys
    @options[:max] = Float::INFINITY if @options[:max] < @options[:min]
  end

  def run(session)
    session.log :info => "Waiting user input", :trace => "Waiting user input: #{@options.to_pretty_s}"
    @options[:play] = PlayCommand.new(@options[:play]).download(session) if @options[:play]
    digits = session.pbx.capture @options

    session.info(digits ? "User pressed: #{digits}" : "User didn't press anything")
    session[:last_capture] = digits
  end
end
