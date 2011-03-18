require(File.expand_path '../../../config/boot.rb', __FILE__)
require(File.expand_path '../../../config/environment.rb', __FILE__)
require(File.expand_path '../../../lib/batphone/lib/fastagi.rb', __FILE__)

class FastAGIServer < FastAGIProtocol
  def agi_post_init
    pbx = AsteriskAdapter.new self
    @log = Rails.logger

    app_id = self['arg_1']
    app = Application.find app_id
    begin
      app.run pbx
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
    p event
  end
end

module Globals
  class << self
    attr_accessor :ami
  end
end

class PbxInterface < MagicObjectProtocol::Server

  def call(address, application_id)
    Globals.ami.originate :channel => address,
      :context => 'verboice',
      :exten => application_id,
      :priority => 1,
      :async => true
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
