#!/usr/bin/env ruby
require(File.expand_path '../../../config/boot.rb', __FILE__)
require(File.expand_path '../../../config/environment.rb', __FILE__)
require 'librevox'

class Globals
  class << self
    attr_accessor :pbx
  end
end

class MyFreeswitchInboundListener < Freeswitch::InboundListener
  def unbind
    done
    EM.add_timer(1) do
      Globals.pbx = Librevox.run MyFreeswitchInboundListener
    end
    super
  end
end

class MyPbxInterface < Freeswitch::PbxInterface
  def post_init
    self.pbx = Globals.pbx
  end
end

EM.error_handler do |err|
  p err
end

EM::run do
  EM.schedule do
    Librevox.start do
      run Freeswitch::OutboundListener, :port => Freeswitch::OutboundListener::Port
      Globals.pbx = run MyFreeswitchInboundListener
    end
    EM::start_server '127.0.0.1', Freeswitch::PbxInterface::Port, MyPbxInterface
    puts 'Ready'
  end
end
EM.reactor_thread.join
