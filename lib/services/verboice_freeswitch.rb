#!/usr/bin/env ruby

ENV["RAILS_ENV"] = ARGV[0] unless ARGV.empty?
$log_path = File.expand_path '../../../log/freeswitch.log', __FILE__

require(File.expand_path '../../../config/boot.rb', __FILE__)
require(File.expand_path '../../../config/environment.rb', __FILE__)
require 'librevox'

BaseBroker.instance = Freeswitch::Broker.new

class Globals
  class << self
    attr_accessor :freeswitch_client
  end
end

class MyFreeswitchClient < Freeswitch::Client
  def unbind
    done
    EM.add_timer(1) do
      Globals.freeswitch_client = Librevox.run MyFreeswitchClient
    end
    super
  end
end

class MyBrokerFacade < BrokerFacade
  def post_init
    BaseBroker.instance.freeswitch_client = Globals.freeswitch_client
  end
end

EM.error_handler do |err|
  p err
  p err.backtrace
end

EM::run do
  EM.schedule do
    Librevox.start do
      run Freeswitch::CallManager, :port => Freeswitch::CallManager::Port
      Globals.freeswitch_client = run MyFreeswitchClient
    end
    EM::start_server '127.0.0.1', Freeswitch::CallManager::Port, Freeswitch::CallManager
    puts 'Ready'
  end
end
EM.reactor_thread.join
