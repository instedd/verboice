#!/usr/bin/env ruby

ENV["RAILS_ENV"] = ARGV[0] unless ARGV.empty?
$log_path = File.expand_path '../../../log/asterisk.log', __FILE__

require(File.expand_path '../../../config/boot.rb', __FILE__)
require(File.expand_path '../../../config/environment.rb', __FILE__)

BaseBroker.instance = Asterisk::Broker.new

EM.error_handler do |err|
  p err
  p err.backtrace
end

EM::run do
  EM.schedule do
    EM::connect 'localhost', Asterisk::Client::Port, Asterisk::Client
    EM::start_server 'localhost', BrokerFacade::Port, BrokerFacade
    EM::start_server 'localhost', Asterisk::CallManager::Port, Asterisk::CallManager
    puts 'Ready'
  end
end
EM.reactor_thread.join
