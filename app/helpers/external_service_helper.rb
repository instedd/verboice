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

module ExternalServiceHelper

  def destroy_message_for(external_service)
    call_flows = external_service.call_flows.pluck(:name)
    if call_flows.size > 0
      "This external service is being used in the following call flows: #{call_flows.join(', ')}. "\
      "Deleting will remove all these steps from each of those call flows. "\
      "Are you sure you want to proceed?"
    else
      "Are you sure?"
    end
  end

end
