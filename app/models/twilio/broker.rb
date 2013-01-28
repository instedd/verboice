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

module Twilio
  class Broker < BaseBroker
    def self.instance
      $twilio_broker ||= new
    end

    def start
      EM.start_server '0.0.0.0', Twilio::Server::Port, Twilio::Server
    end

    def pbx_available?
      true
    end

    def create_channel(channel)
    end

    def delete_channel(channel)
    end

    def channels
      Channels::Twilio.scoped
    end
  end
end
