#!/usr/bin/env ruby

ENV["RAILS_ENV"] = ARGV[0] unless ARGV.empty?
$log_path = File.expand_path '../../../log/asterisk.log', __FILE__

require(File.expand_path '../../../config/boot.rb', __FILE__)
require(File.expand_path '../../../config/environment.rb', __FILE__)

BaseBroker.instance = Asterisk::Broker.new

module Globals
  class << self
    attr_accessor :asterisk_client
  end
end

class MyAsteriskClient < Asterisk::Client
  def unbind
    EM.add_timer(1) do
      Globals.asterisk_client = EM::connect '127.0.0.1', Port, MyAsteriskClient
    end
    super
  end
end

class MyBrokerFacade < BrokerFacade
  def post_init
    BaseBroker.instance.asterisk_client = Globals.asterisk_client
  end
end

EM.error_handler do |err|
  p err
  p err.backtrace
end

EM::run do
  EM.schedule do
    Globals.asterisk_client = EM::connect 'localhost', Asterisk::Client::Port, MyAsteriskClient
    EM::start_server 'localhost', BrokerFacade::Port, MyBrokerFacade
    EM::start_server 'localhost', Asterisk::CallManager::Port, Asterisk::CallManager
    puts 'Ready'
  end
end
EM.reactor_thread.join
