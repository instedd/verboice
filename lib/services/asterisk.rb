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

class PbxInterface < EventMachine::Connection
  include EventMachine::Protocols::ObjectProtocol

  def receive_object(obj)
    method, args = obj.first
    self.send(method, *args)
  end

  def call(address, application_id)
    EM.schedule do
      f = Fiber.new do
        puts address

        response = Globals.ami.originate :channel => address,
                      :context => 'verboice',
                      :exten => application_id,
                      :priority => 1,
                      :async => true
        p response
      end
      f.resume
    end
  end

end


EM.error_handler do |err|
  p err
end


EM::run do
  EM::start_server '127.0.0.1', 19000, FastAGIServer
  Globals.ami = EM::connect '127.0.0.1', 5038, AmiClient
  EM::start_server '127.0.0.1', 8787, PbxInterface
  puts 'Ready'

end
