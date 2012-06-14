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

module Freeswitch
  class CallManager < Librevox::Listener::Outbound
    Port = Rails.configuration.freeswitch_configuration[:call_manager_port].to_i

    event :channel_hangup do |event|
      @current_session.quit! if @current_session
    end

    def session_initiated
      pbx = Freeswitch::Adapter.new self
      begin
        @current_session = pbx.new_session
        @current_session.run
      rescue Exception => ex
        puts "FATAL: #{ex.inspect}"
        close_connection
      ensure
        done
      end
    end
  end
end
