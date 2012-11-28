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
  class AmiProtocol < EventMachine::Protocols::LineAndTextProtocol
    def receive_line(line)
      return @first_line_consumed = true unless @first_line_consumed

      if @packet
        line = line.strip
        if line.empty?
          type = @packet.delete :type
          if type == :response
            resume_fiber_with @packet
          else
            Fiber.new { receive_event @packet }.resume
          end
          @packet = nil
        else
          key, value = line.split(':', 2)
          if key && value
            key = key.downcase.to_sym
            @packet[key] = value.strip
          end
        end
      elsif line =~ /^Response: (.*)/i
        @packet = {:type => :response, :response => $1}
      elsif line =~ /^Event: (.*)/i
        @packet = {:type => :event, :event => $1}
      else
        close_connection
        raise "Error in AMI protocol, received: #{line}"
      end
    end

    def resume_fiber_with(packet)
      @fiber.resume @packet
    end

    def method_missing(name, *args)
      @fiber = Fiber.current

      send_data "action: #{name}\n"
      args[0].each { |key, value| send_data "#{key}: #{value}\n" } if args[0]
      send_data "\n"

      Fiber.yield
    end
  end
end
