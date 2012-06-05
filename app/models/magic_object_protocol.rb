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

module MagicObjectProtocol

  class Server < EventMachine::Connection
    include EventMachine::Protocols::ObjectProtocol

    def receive_object(obj)
      method, args = obj.first
      f = Fiber.new do
        begin
          response = self.send(method, *args)
          send_object response
        rescue Exception => ex
          send_object ex
        end
      end
      f.resume
    end
  end

  module Client
    include EventMachine::Protocols::ObjectProtocol

    def method_missing(name, *args)
      @mutex ||= Mutex.new
      @cv ||= ConditionVariable.new
      send_object name => args
      @mutex.synchronize do
        timeout(10) { @cv.wait @mutex unless @obj }
      end
      raise @obj if @obj.is_an? Exception
      obj, @obj = @obj, nil
      obj
    end

    def receive_data(data)
      super
    rescue Exception => ex
      receive_object(ex)
    end

    def receive_object(obj)
      @mutex.synchronize do
        @obj = obj
        @cv.signal
      end
    end

    def unbind
      @mutex.synchronize do
        @obj = Exception.new 'Cannot connect to Broker'
        @cv.signal
      end
    end
  end

end
