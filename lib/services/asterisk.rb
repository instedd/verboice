require(File.expand_path '../../../config/boot.rb', __FILE__)
require(File.expand_path '../../../config/environment.rb', __FILE__)
require(File.expand_path '../../../lib/batphone/lib/fastagi.rb', __FILE__)

class FastAGIServer < FastAGIProtocol
  def agi_post_init
    pbx = AsteriskAdapter.new self
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
end

module Globals
  class << self
    attr_accessor :ami
  end
end

class PbxInterface < MagicObjectProtocol::Server

  def call(address, application_id, call_log_id)
    result = Globals.ami.originate :channel => address,
      :application => 'AGI',
      :data => "agi://localhost:19000,#{application_id},#{call_log_id}",
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
    EM::start_server '127.0.0.1', 19000, FastAGIServer
    Globals.ami = EM::connect '127.0.0.1', 5038, AmiClient
    EM::start_server '127.0.0.1', 8787, PbxInterface
    puts 'Ready'
  end
end
EM.reactor_thread.join
