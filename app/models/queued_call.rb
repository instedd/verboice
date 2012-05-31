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

  serialize :flow, Command

  def start
    call_log.start_outgoing address
    new_session
  end

  def new_session
    options = {:call_log => call_log, :address => address}

    if callback_url.present?
      options[:call_flow] = CallFlow.new :callback_url => callback_url
    elsif flow.present?
      options[:call_flow] = CallFlow.new :flow => flow
    end

    if status_callback_url.present? && options[:call_flow]
      options[:call_flow].project = Project.new status_callback_url: status_callback_url
    end

    channel.new_session options
  end
end
