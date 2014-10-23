class CallQueuingError < RuntimeError
  def self.invalid_date error_message
    CallQueuingError.new "Invalid date: #{error_message}"
  end

  def self.unsuported_time_zone time_zone
    CallQueuingError.new "Time zone #{time_zone} not supported"
  end
end
