#!/usr/bin/env ruby

ENV["RAILS_ENV"] = ARGV[0] unless ARGV.empty?
$log_path = File.expand_path '../../../log/voxeo.log', __FILE__

require(File.expand_path '../../../config/boot.rb', __FILE__)
require(File.expand_path '../../../config/environment.rb', __FILE__)

Rails.logger = Logger.new(STDOUT) if STDOUT.tty?

BaseBroker.instance = Voxeo::Broker.new

broker_port = Rails.configuration.verboice_configuration[:voxeo_broker_port].to_i

EM.error_handler do |err|
  p err
  p err.backtrace
end

EM::run do
  EM.schedule do
    EM::start_server 'localhost', broker_port, BrokerFacade
    EM.start_server '0.0.0.0', Voxeo::Server::Port, Voxeo::Server
    puts 'Ready'
  end
end
EM.reactor_thread.join