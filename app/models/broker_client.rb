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

require_dependency 'pbx_unavailable_exception'

class BrokerClient

  def initialize port
    @port = port
  end

  def open
    client = EM.connect '127.0.0.1', @port, MagicObjectProtocol::Client
    begin
      yield client
    ensure
      client.close_connection
    end
  end

  def method_missing(name, *args)
    open do |client|
      client.send name, *args
    end
  end
end
