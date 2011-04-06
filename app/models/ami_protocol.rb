class AmiProtocol < EventMachine::Protocols::LineAndTextProtocol

  def receive_line(line)
    return @first_line_consumed = true unless @first_line_consumed

    if @packet
      line = line.strip
      if line.empty?
        type = @packet.delete :type
        if type == :response
          resume_fiber_with @packet
        else
          receive_event @packet
        end
        @packet = nil
      else
        key, value = line.split(':', 2)
        if key && value
          key = key.downcase.to_sym
          @packet[key] = value.strip
        end
      end
    elsif line =~ /^Response: (.*)/i
      @packet = {:type => :response, :response => $1}
    elsif line =~ /^Event: (.*)/i
      @packet = {:type => :event, :event => $1}
    else
      close_connection
      raise "Error in AMI protocol, received: #{line}"
    end
  end

  def resume_fiber_with(packet)
    @fiber.resume @packet
  end

  def method_missing(name, *args)
    @fiber = Fiber.current

    send_data "action: #{name}\n"
    args[0].each { |key, value| send_data "#{key}: #{value}\n" }
    send_data "\n"

    Fiber.yield
  end
end
