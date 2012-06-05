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

class Commands::DialCommand < Command
  attr_accessor :number
  attr_accessor :channel_name
  attr_accessor :caller_id

  def initialize(number, options = {})
    @number = number
    @channel_name = options[:channel]
    @caller_id = options[:caller_id]
  end

  def run(session)
    if @channel_name.present?
      channel = session.channel.account.channels.find_by_name @channel_name
    else
      channel = session.channel
    end

    address = BaseBroker.instance.get_dial_address channel, @number
    session.info "Dialing #{address}"

    @options = {}
    @options[:caller_id] = caller_id if caller_id
    session[:dial_status] = session.pbx.dial address, @options
    session.info "Dial completed with status '#{session[:dial_status]}'"

    super
  end
end