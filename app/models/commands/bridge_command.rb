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

class Commands::BridgeCommand < Command
  attr_accessor :session_id

  def initialize(session_id)
    @session_id = session_id.to_i
  end

  def run(session)
    session.trace "Bridging with session #{@session_id}.", command: 'bridge', action: 'start'
    other_session = session.broker.find_session_by_call_log_id @session_id
    session.pbx.bridge_with other_session
    session.trace "Bridge with session #{@session_id} completed.", command: 'bridge', action: 'finish'
  end

end