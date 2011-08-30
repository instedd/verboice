module Freeswitch
  class Client < Librevox::Listener::Inbound
    event :background_job do |event|
      if event.body.match /^-ERR (.*)/
        error_message = $1
        args = CGI.unescape event.content[:job_command_arg]
        if args.match /verboice_call_log_id=(\d+)/
          call_log = CallLog.find($1) or return
          call_log.error "Failed to establish the communication: #{error_message}"
          call_log.finish :failed
        end
      end
    end
  end
end
