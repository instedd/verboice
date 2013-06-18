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

module Asterisk
  class Client < Asterisk::AmiProtocol
    Port = Rails.configuration.asterisk_configuration[:ami_port].to_i

    def post_init
      Fiber.new do
        response = self.login :username => 'verboice', :secret => 'verboice'
        if response[:response] != 'Success'
          Rails.logger.error "Login to Asterisk AMI failed: #{response}"
          close_connection
        else
          Rails.logger.info response
          $asterisk_client = self
          self.events eventmask: 'on'
          EM.fiber_sleep 5
          $asterisk_client_connect_handler.call if $asterisk_client_connect_handler
        end
      end.resume
    end

    def unbind
      EM.add_timer(1) do
        EM::connect '127.0.0.1', Port, self.class
      end
      super
    end

    def self.on_event(&block)
      $asterisk_client_event_handler = block
    end

    def self.on_connect(&block)
      $asterisk_client_connect_handler = block
    end

    def receive_event(event)
      if $asterisk_client_event_handler
        $asterisk_client_event_handler.call event
      end
    end
  end
end
