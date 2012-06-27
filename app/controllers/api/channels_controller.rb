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
module Api
  class ChannelsController < ApiController

    def create
      data = request.raw_post
      data = JSON.parse(data).with_indifferent_access
      channel = Channel.from_json data
      channel.account = current_account
      channel.call_flow = current_account.call_flows.find_by_name(data[:call_flow])
      if channel.save
        render :json => channel
      else
        render :json => errors_to_json(channel, 'creating')
      end
    end

    def destroy
      chan = current_account.channels.find_by_name params[:name]

      return head :not_found unless chan

      chan.destroy
      head :ok
    end

    def list
      channel_names = current_account.channels.map &:name
      render :json => channel_names
    end
  end
end