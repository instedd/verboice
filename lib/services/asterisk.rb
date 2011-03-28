require(File.expand_path '../../../config/boot.rb', __FILE__)
require(File.expand_path '../../../config/environment.rb', __FILE__)
require(File.expand_path '../../../lib/batphone/lib/fastagi.rb', __FILE__)

AsteriskConfig = Rails.configuration.asterisk_configuration
AmiPort = AsteriskConfig[:ami_port].to_i
FastAgiPort = AsteriskConfig[:fast_agi_port].to_i

PbxInterfacePort = Rails.configuration.verboice_configuration[:pbx_interface_port].to_i

module Globals
  class << self
    attr_accessor :pbx
  end
end

class MyAmiClient < Asterisk::AmiClient
  def unbind
    EM.add_timer(1) do
      Globals.pbx = EM::connect '127.0.0.1', AmiPort, self.class
    end
    super
  end
end

class MyPbxInterface < Asterisk::PbxInterface
  def post_init
    self.pbx = Globals.pbx
  end
end

EM.error_handler do |err|
  p err
end

EM::run do
  EM.schedule do
    EM::start_server 'localhost', FastAgiPort, Asterisk::FastAGIServer
    Globals.pbx = EM::connect 'localhost', AmiPort, MyAmiClient
    EM::start_server 'localhost', PbxInterfacePort, MyPbxInterface
    puts 'Ready'
  end
end
EM.reactor_thread.join
