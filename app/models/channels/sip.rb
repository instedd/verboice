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

  def port
    Rails.configuration.verboice_configuration[:local_pbx_broker_port].to_i
  end

  def register?
    register
  end

  def outbound?
    direction == 'outbound' || direction == 'both'
  end

  def inbound?
    direction == 'inbound' || direction == 'both'
  end

  def asterisk_address_string_for broker, address
    broker.sip_address_string_for self, address
  end

  def server_username_uniqueness
    conflicting_channels = Channels::CustomSip.where('id != ?', id).all.any? { |c| c.username == self.username && c.domain == self.domain }
    errors.add(:base, 'Username and domain have already been taken') if conflicting_channels
  end

  def errors_count
    status = broker_client.channel_status(id)[id]
    status && !status[:ok] ? status[:messages].length : 0
  end
end