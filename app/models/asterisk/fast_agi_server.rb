class FastAGIProtocol; end;

module Asterisk
  class FastAGIServer < FastAGIProtocol
    Port = Rails.configuration.asterisk_configuration[:fast_agi_port].to_i

    def agi_post_init
      @log = Rails.logger

      pbx = Asterisk::Adapter.new self

      app_id = self['arg_1']
      call_log_id = self['arg_2']
      app = Application.find app_id
      call_log = CallLog.find call_log_id if call_log_id
      begin
        app.run pbx, call_log
      rescue Exception => ex
        puts "FATAL: #{ex.inspect}"
      ensure
        close_connection
      end
    end
  end
end
