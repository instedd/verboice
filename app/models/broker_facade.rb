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

class BrokerFacade < MagicObjectProtocol::Server

  def notify_call_queued(channel_id, not_before = nil)
    unless not_before
      channel = Channel.find channel_id
      BaseBroker.instance.notify_call_queued channel
    end
    nil
  end

  def create_channel(channel_id)
    BaseBroker.instance.create_channel channel_id
  end

  def update_channel(channel_id)
    BaseBroker.instance.update_channel channel_id
  end

  def destroy_channel(channel_id)
    BaseBroker.instance.destroy_channel channel_id
  end

  def active_calls_count_for(channel_id)
    channel = Channel.find channel_id
    BaseBroker.instance.active_calls_count_for channel
  end

  def redirect(session_id, options)
    BaseBroker.instance.redirect session_id, options
  end

  def channel_status(*channel_ids)
    BaseBroker.instance.channel_status *channel_ids
  end
end
