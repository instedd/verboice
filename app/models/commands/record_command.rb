class Commands::RecordCommand < Command

  attr_accessor :filename, :stop_keys, :timeout

  def initialize filename, options = {}
    @filename = filename
    @stop_keys = options[:stop_keys] || '01234567890*#'
    @timeout = options[:timeout].try(:to_i) || 10
  end

  def run(session)
    session.info "Record user voice"
    session.pbx.record filename, stop_keys, timeout
    super
  end
end
