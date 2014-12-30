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

class Commands::NuntiumCommand < Command

  def initialize(resource_guid, channel_id, rcpt_type, expr = nil)
    @resource_guid = resource_guid
    @channel_id = channel_id
    @rcpt_type = rcpt_type   # can be :caller or :expr
    @expr = expr
  end

  def serialize_parameters
    params = {
      expr: @expr,
      resource_guid: @resource_guid,
      rcpt_type: @rcpt_type
    }

    if @channel_id.present?
      params[:channel_id] = @channel_id
    end

    params
  end

end
