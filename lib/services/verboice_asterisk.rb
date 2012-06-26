#!/usr/bin/env ruby

# Copyright (C) 2010-2012, InSTEDD
#
# This file is part of Verboice.
#
# Verboice is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# Verboice is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with Verboice.  If not, see <http://www.gnu.org/licenses/>.

ENV["RAILS_ENV"] = ARGV[0] unless ARGV.empty?
$log_path = File.expand_path '../../../log/asterisk.log', __FILE__

require(File.expand_path '../../../config/boot.rb', __FILE__)
require(File.expand_path '../../../config/environment.rb', __FILE__)

Rails.logger = Logger.new(STDOUT) if STDOUT.tty?

BaseBroker.instance = Asterisk::Broker.new

broker_port = Rails.configuration.verboice_configuration[:local_pbx_broker_port].to_i

EM.error_handler do |err|
  p err
  p err.backtrace
end

EM::run do
  EM.schedule do
    EM::connect 'localhost', Asterisk::Client::Port, Asterisk::Client
    EM::start_server 'localhost', broker_port, BrokerFacade
    EM::start_server 'localhost', Asterisk::CallManager::Port, Asterisk::CallManager
    BaseBroker.instance.start
    puts 'Ready'
  end
end
EM.reactor_thread.join
