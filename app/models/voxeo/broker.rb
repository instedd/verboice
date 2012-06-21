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

module Voxeo
  class Broker < BaseBroker

    # Voxeo calls up to 3 times and waits the user to pick up
    # the call before sending the response, set a large timeout
    TIMEOUT = 120

    def call session
      http = EventMachine::HttpRequest.new(session.channel.url)
      http = http.get :timeout => TIMEOUT, :query => {:tokenid => session.channel.token, :callsid => session.id, :numbertodial => session.address} #TODO AR: we can add a callerid param here

      http.callback do
        begin
          if http.response_header.status.to_i != 200
            raise "Voxeo call request failed with status #{http.response_header.status}"
          end

          body = http.response

          session.trace "Voxeo call request returned: #{body}"

          # Voxeo sends failures as "failure: reason" with http status 200, so...
          if body.start_with? "failure"
            raise "Voxeo call request returned failure, reason: #{body}"
          end
        rescue Exception => e
          session.trace e
          call_request_failed session
        end
      end

      http.errback do
        session.trace (http.error.present? ? http.error : "Failed to communicate with Voxeo")
        call_request_failed session
      end
    end

    def pbx_available?
      true
    end

    def create_channel(channel)
    end

    def delete_channel(channel)
    end

    def channels
      Channel.where("kind = 'voxeo'")
    end

    private

    def call_request_failed session
      Fiber.new do
        # TODO AR: we can try to set a meaningful reason
        call_rejected session.id, :error
      end.resume
    end

  end
end