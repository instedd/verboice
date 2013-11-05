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

class Channels::Sip < Channel
  validate :server_username_uniqueness

  config_accessor :username
  config_accessor :password
  config_accessor :domain
  config_accessor :direction
  config_accessor :register
  config_accessor :number

  def register?
    register == true || register == "1"
  end

  def outbound?
    direction == 'outbound' || direction == 'both'
  end

  def inbound?
    direction == 'inbound' || direction == 'both'
  end

  def server_username_uniqueness
    return unless self.username.present?
    conflicting_channels = Channels::CustomSip
    conflicting_channels = conflicting_channels.where('id != ?', id) if id
    conflicting_channels = conflicting_channels.all.any? { |c| c.username == self.username && c.domain == self.domain }
    errors.add(:base, 'Username and domain have already been taken') if conflicting_channels
  end

  def errors_count
    status = BrokerClient.channel_status(id)[id]
    status && !status[:ok] ? status[:messages].length : 0
  end
end
