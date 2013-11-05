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

class Commands::RecordCommand < Command
  attr_accessor :filename, :stop_keys, :timeout

  def initialize key, description, options = {}
    @key         = key
    @description = description
    @stop_keys   = options[:stop_keys] || '01234567890*#'
    @timeout     = options[:timeout].try(:to_i) || 10
  end

  def serialize_parameters
    {
      key: @key,
      description: @description,
      stop_keys: @stop_keys,
      timeout: @timeout
    }
  end
end
