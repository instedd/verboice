require(File.expand_path '../../../config/boot.rb', __FILE__)
require(File.expand_path '../../../config/environment.rb', __FILE__)
require 'librevox'

OutboundListenerPort = Rails.configuration.freeswitch_configuration[:outbound_listener_port].to_i
PbxInterfacePort = Rails.configuration.verboice_configuration[:pbx_interface_port].to_i

class FreeswitchOutboundListener < Librevox::Listener::Outbound
  event :channel_hangup do |event|
    @session.quit!
  end

  def session_initiated
    pbx = FreeswitchAdapter.new self

    app_id = session[:variable_verboice_application_id]
    call_log_id = session[:variable_verboice_call_log_id]
    app = Application.find app_id
    call_log = CallLog.find call_log_id if call_log_id
    begin
      @session = app.new_session pbx, call_log
      @session.run
    rescue Exception => ex
      puts "FATAL: #{ex.inspect}"
      close_connection
    ensure
      done
    end
  end
end

class FreeswitchInboundListener < Librevox::Listener::Inbound

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

  def unbind
    done
    EM.add_timer(1) do
      Globals.pbx = Librevox.run FreeswitchInboundListener
    end
    super
  end

end

class Globals
  class << self
    attr_accessor :pbx
  end
end

class PbxInterface < MagicObjectProtocol::Server

  def post_init
    @pbx = Globals.pbx
  end

  def call(address, application_id, call_log_id)
    raise "PBX is not available" if @pbx.error?
    vars = "{verboice_application_id=#{application_id},verboice_call_log_id=#{call_log_id}}"
    @pbx.command "bgapi originate #{vars}#{address} '&socket(localhost:#{OutboundListenerPort} sync full)'"
    nil
  end

end

EM::run do
  EM.schedule do
    Librevox.start do
      run FreeswitchOutboundListener, :port => OutboundListenerPort
      Globals.pbx = run FreeswitchInboundListener
    end
    EM::start_server '127.0.0.1', PbxInterfacePort, PbxInterface
    puts 'Ready'
  end
end
EM.reactor_thread.join
