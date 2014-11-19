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
  def initialize(call_log_id)
    @call_log_id = call_log_id
  end

  def perform
    call_log = CallLog.find @call_log_id
    contact_address = ContactAddress.find_by_address call_log.address
    contact = contact_address.contact
    persisted_vars = contact.persisted_variables.includes(:project_variable).all

    vars = {}
    persisted_vars.each do |var|
      vars[var.project_variable.name] = var.value
    end

    request = {
      project_id: call_log.project_id,
      call_flow_id: call_log.call_flow_id,
      address: call_log.address,
      vars: vars,
    }

    HubClient.current.notify "verboice", "call_finished", request.to_json
  end
end
