class CaptureCommand < Command
  param :min, :integer, :default => 1, :ui_length => 1
  param :max, :integer, :default => 1, :ui_length => 1
  param :finish_on_key, :string, :default => '#', :ui_length => 1
  param :timeout, :integer, :default => 5, :ui_length => 1
  param :play, :string, :ui_length => 80

  def initialize(options = {})
    @options = {:min => 1, :max => 1, :finish_on_key => '#', :timeout => 5}
    @options.merge! options
    @options[:max] = Float::INFINITY if @options[:max] < @options[:min]
  end

  def run(session)
    session.log :info => "Waiting user input", :trace => "Waiting user input: #{@options.to_pretty_s}"

    options = @options.dup
    if options[:play].present?
      options[:play] = PlayUrlCommand.new(options[:play]).setup_file(session)
    else
      options.delete :play
    end

    [:digits, :timeout, :finish_key].each { |key| session.delete key }

    digits = session.pbx.capture options
    case digits
    when nil
      session.info("User didn't press enough digits")
      session[:timeout] = true
    when :timeout
      session.info("User timeout")
      session[:timeout] = true
    when :finish_key
      session.info("User pressed the finish key")
      session[:finish_key] = true
    else
      session.info("User pressed: #{digits}")
      session[:digits] = digits
    end
  end
end
