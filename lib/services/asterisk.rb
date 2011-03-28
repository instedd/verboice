require(File.expand_path '../../../config/boot.rb', __FILE__)
require(File.expand_path '../../../config/environment.rb', __FILE__)
require(File.expand_path '../../../lib/batphone/lib/fastagi.rb', __FILE__)

AsteriskConfig = Rails.configuration.asterisk_configuration
AmiPort = AsteriskConfig[:ami_port].to_i
FastAgiPort = AsteriskConfig[:fast_agi_port].to_i

PbxInterfacePort = Rails.configuration.verboice_configuration[:pbx_interface_port].to_i

class FastAGIServer < FastAGIProtocol
  def agi_post_init
    pbx = Asterisk::Adapter.new self
    @log = Rails.logger

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

class AmiClient < AmiProtocol
  def post_init
    Fiber.new do
      response = self.login :username => 'verboice', :secret => 'verboice'
      if response[:response] != 'Success'
        puts "Login failed"
      else
        puts response
      end
    end.resume
  end

  def receive_event(event)
    if event[:event] == 'OriginateResponse' && event[:response] == 'Failure'
      call_log = CallLog.find(event[:actionid]) or return
      call_log.error 'Failed to establish the communication'
      call_log.finish :failed
    end
  end

  def unbind
    EM.add_timer(1) do
      Globals.pbx = EM::connect '127.0.0.1', AmiPort, AmiClient
    end
    super
  end
end

module Globals
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
    result = @pbx.originate :channel => address,
      :application => 'AGI',
      :data => "agi://localhost:#{FastAgiPort},#{application_id},#{call_log_id}",
      :async => true,
      :actionid => call_log_id
    raise result[:message] if result[:response] == 'Error'
    nil
  end

end

EM.error_handler do |err|
  p err
end

EM::run do
  EM.schedule do
    EM::start_server 'localhost', FastAgiPort, FastAGIServer
    Globals.pbx = EM::connect 'localhost', AmiPort, AmiClient
    EM::start_server 'localhost', PbxInterfacePort, PbxInterface
    puts 'Ready'
  end
end
EM.reactor_thread.join
