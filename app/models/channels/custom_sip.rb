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

class Channels::CustomSip < Channels::Sip

  validates_numericality_of :limit, :only_integer => true, :greater_than => 0, :if => :has_limit?

  config_accessor :host
  config_accessor :register
  config_accessor :direction
  config_accessor :limit

  def servers
    hosts = config['host'] || []
    servers = []
    hosts.each_with_index do |host, i|
      servers << Server.new(host, nil, config['register'][i], config['direction'][i])
    end
    servers.length == 0 ? [Server.new] : servers
  end

  def self.can_handle? a_kind
    a_kind == 'sip'
  end

  def self.kind
    'Sip'
  end

  def server_username_uniqueness
    conflicting_channels = Channels::CustomSip.all.select{|c| c.username == self.username && c.id != self.id && !(c.host & self.host).empty?}
    errors.add(:base, 'Username and host have already been taken') unless conflicting_channels.empty?
  end

end