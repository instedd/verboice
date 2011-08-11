#!/usr/bin/env ruby

ENV["RAILS_ENV"] = ARGV[0] unless ARGV.empty?
$log_path = File.expand_path '../../../log/asterisk.log', __FILE__

require(File.expand_path '../../../config/boot.rb', __FILE__)
require(File.expand_path '../../../config/environment.rb', __FILE__)
require(File.expand_path '../../../lib/batphone/lib/fastagi.rb', __FILE__)

module Globals
  class << self
    attr_accessor :pbx
  end
end

class MyAmiClient < Asterisk::AmiClient
  def unbind
    EM.add_timer(1) do
      Globals.pbx = EM::connect '127.0.0.1', Port, MyAmiClient
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
  p err.backtrace
end

EM::run do
  EM.schedule do
    EM::start_server 'localhost', Asterisk::FastAGIServer::Port, Asterisk::FastAGIServer
    Globals.pbx = EM::connect 'localhost', Asterisk::AmiClient::Port, MyAmiClient
    EM::start_server 'localhost', Asterisk::PbxInterface::Port, MyPbxInterface
    puts 'Ready'
  end
end
EM.reactor_thread.join
