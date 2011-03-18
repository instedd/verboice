class AmiProtocol < EventMachine::Protocols::LineAndTextProtocol
  def receive_line(line)
    puts line
  end

  def method_missing(name, *args)
    send_data "action: #{name}\n"
    args[0].each { |key, value| send_data "#{key}: #{value}\n" }
    send_data "\n"
  end
end
