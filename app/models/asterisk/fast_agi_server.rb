class FastAGIProtocol < EventMachine::Protocols::LineAndTextProtocol; end

module Asterisk
  class FastAGIServer < FastAGIProtocol
    Port = Rails.configuration.asterisk_configuration[:fast_agi_port].to_i

    def agi_post_init
      @log = Rails.logger

      pbx = Asterisk::Adapter.new self
      begin
        pbx.run
      rescue Exception => ex
        puts "FATAL: #{ex.inspect}"
      ensure
        close_connection
      end
    end
  end
end
