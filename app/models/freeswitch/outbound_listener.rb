module Freeswitch
  class OutboundListener < Librevox::Listener::Outbound
    Port = Rails.configuration.freeswitch_configuration[:outbound_listener_port].to_i

    event :channel_hangup do |event|
      @current_session.quit! if @current_session
    end

    def session_initiated
      pbx = Freeswitch::Adapter.new self

      app_id = session[:variable_verboice_application_id]
      call_log_id = session[:variable_verboice_call_log_id]
      app = Application.find app_id
      call_log = CallLog.find call_log_id if call_log_id
      begin
        @current_session = app.new_session pbx, call_log
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
