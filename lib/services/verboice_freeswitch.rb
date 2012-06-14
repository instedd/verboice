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
