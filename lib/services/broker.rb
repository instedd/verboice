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
$log_path = File.expand_path '../../../log/broker.log', __FILE__

require(File.expand_path '../../../config/boot.rb', __FILE__)
require(File.expand_path '../../../config/environment.rb', __FILE__)

Rails.logger = Logger.new(STDOUT) if STDOUT.tty?

EM.error_handler do |err|
  p err
  p err.backtrace
end

EM::run do
  EM.schedule do
    # Asterisk
    EM::connect 'localhost', Asterisk::Client::Port, Asterisk::Client
    EM::start_server 'localhost', Asterisk::CallManager::Port, Asterisk::CallManager

    # Voxeo
    EM.start_server '0.0.0.0', Voxeo::Server::Port, Voxeo::Server

    # Generic
    EM::start_server 'localhost', BrokerFacade::PORT, BrokerFacade

    Asterisk::Broker.instance.start
    Voxeo::Broker.instance.start

    puts 'Ready'
  end
end
EM.reactor_thread.join
