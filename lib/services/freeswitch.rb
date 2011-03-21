require(File.expand_path '../../../config/boot.rb', __FILE__)
require(File.expand_path '../../../config/environment.rb', __FILE__)
require 'librevox'

class FreeswitchOutboundListener < Librevox::Listener::Outbound
  #event :channel_hangup do
    #done
  #end

  def session_initiated
    pbx = FreeswitchAdapter.new self

    app_id = session[:variable_verboice_application_id]
    call_log_id = session[:variable_verboice_call_log_id]
    app = Application.find app_id
    call_log = CallLog.find call_log_id if call_log_id
    begin
      app.run pbx, call_log
    rescue Exception => ex
      puts "FATAL: #{ex.inspect}"
      close_connection
    ensure
      done
    end
  end
end

class FreeswitchInboundListener < Librevox::Listener::Inbound
end

class Globals
  class << self
    attr_accessor :freeswitch
  end
end

class PbxInterface < MagicObjectProtocol::Server

  def call(address, application_id, call_log_id)
    vars = "{verboice_application_id=#{application_id},verboice_call_log_id=#{call_log_id}}"
    Globals.freeswitch.command "api originate #{vars}#{address} '&socket(localhost:9876 sync full)'"
  end

end

EM::run do
  EM.schedule do
    Librevox.start do
      run FreeswitchOutboundListener, :port => "9876"
      Globals.freeswitch = run FreeswitchInboundListener
    end
    EM::start_server '127.0.0.1', 8787, PbxInterface
    puts 'Ready'
  end
end
EM.reactor_thread.join
