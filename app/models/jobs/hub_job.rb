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

class Jobs::HubJob
  def initialize(payload)
    @payload = payload
  end

  def perform
    HubClient.current.notify "projects/#{@payload[:project_id]}/call_flows/#{@payload[:call_flow_id]}/$events/call_done", @payload.to_json

    if @payload[:status] == :completed
      # The endpoint call_finished was only receiving completed (successful) calls events, so we're
      # deprecating it in favour of call_done/completed/failed (see #686)
      #
      # We'll briefly keep this endpoint for not breaking existing applications until they're migrated
      HubClient.current.notify "projects/#{@payload[:project_id]}/call_flows/#{@payload[:call_flow_id]}/$events/call_finished", @payload.to_json

      HubClient.current.notify "projects/#{@payload[:project_id]}/call_flows/#{@payload[:call_flow_id]}/$events/call_completed", @payload.to_json
    else
      HubClient.current.notify "projects/#{@payload[:project_id]}/call_flows/#{@payload[:call_flow_id]}/$events/call_failed", @payload.to_json
    end
  end
end
