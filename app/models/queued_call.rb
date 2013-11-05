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

class QueuedCall < ActiveRecord::Base
  belongs_to :channel
  belongs_to :call_log
  belongs_to :schedule
  belongs_to :project
  belongs_to :call_flow

  serialize :flow, Command::BrokerFlow
  serialize :variables, Hash
  serialize :callback_params, Hash

  def cancel_call!
    call_log.state = :cancelled
    call_log.save!
  end

  def notify_broker
    if channel
      begin
        channel.notify_broker
      rescue Exception => ex
        Rails.logger.info "Error notifying queued call #{id}: #{ex}"
      end
    else
      destroy
    end
  end

  def has_retries_left?
    schedule && schedule.retry_delays.count > retries
  end

  def next_retry_time
    sleep = schedule.retry_delays[retries - 1].to_f * (Rails.env == 'development' ? 1.second : 1.hour)

    schedule.with_time_zone(time_zone) do |time_zoned_schedule|
      time_zoned_schedule.next_available_time(Time.now.utc + sleep)
    end
  end
end
