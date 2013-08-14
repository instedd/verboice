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

class Commands::HangupAndCallbackCommand < Command
  def initialize(options = {})
    @dial_prefix = options[:dial_prefix]
  end

  def run(session)
    session.info "Enqueuing call", command: 'hangup_and_callback', action: 'enqueue_call'
    session.address = dial_address(session)
    session.channel.enqueue_call_to session.address, not_before: 15.seconds.from_now, session_id: session.id
    session.info "Suspending session", command: 'hangup_and_callback', action: 'suspend'
    session.suspend
    session.pbx.hangup
    super
  end

  def dial_address(session)
    if @dial_prefix.present?
      if session.address.start_with?(@dial_prefix)
        session.address
      else
        @dial_prefix + session.address
      end
    else
      session.address
    end
  end
end
