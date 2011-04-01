module Freeswitch
  class OutboundListener < Librevox::Listener::Outbound
    Port = Rails.configuration.freeswitch_configuration[:outbound_listener_port].to_i

    event :channel_hangup do |event|
      @current_session.quit! if @current_session
    end

    def session_initiated
      pbx = Freeswitch::Adapter.new self
      begin
        @current_session = pbx.new_session
        @current_session.run
      rescue Exception => ex
        puts "FATAL: #{ex.inspect}"
        close_connection
      ensure
        done
      end
    end
  end
end
