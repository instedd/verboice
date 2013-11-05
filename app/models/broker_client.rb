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

class BrokerClient
  PORT = Rails.configuration.verboice_configuration[:broker_port].to_i
  @client = BERTRPC::Service.new('127.0.0.1', PORT)

  def self.invalidate_cache(entity, id)
    @client.cast.facade.invalidate_cache(entity, id) rescue nil
  end

  def self.method_missing(name, *args)
    @client.call.facade.send name, *args
  end
end
