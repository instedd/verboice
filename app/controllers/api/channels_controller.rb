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

    def get
      channel = current_account.channels.find_by_name params[:name]

      if channel.present?
        render :json => channel
      else
        head :not_found
      end
    end

    def get_by_id
      id = params[:id]
      channel = current_account.channels.find_by_id(id) || current_account.shared_channels.find_by_model_id(id).try(:channel)

      if channel.present?
        render :json => channel.to_json(account_id: current_account.id)
      else
        head :not_found
      end
    end

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

    def update
      channel = current_account.channels.find_by_name params[:name]

      if channel.present?
        data = request.raw_post
        data = JSON.parse(data).with_indifferent_access
        channel.from_json data
        if channel.save
          render :json => channel
        else
          render :json => errors_to_json(channel, 'updating')
        end
      else
        head :not_found
      end
    end

    def destroy
      channel = current_account.channels.find_by_name params[:name]

      if channel.present?
        channel.destroy
        head :ok
      else
        head :not_found
      end
    end

    def enable
      channel = current_account.channels.find_by_id(params[:id])
      if channel.present?
        channel.enable!
        head :ok
      else
        head :not_found
      end
    end

    def disable
      channel = current_account.channels.find_by_id(params[:id])
      if channel.present?
        channel.disable!
        head :ok
      else
        head :not_found
      end
    end

    def list
      channel_names = current_account.channels.map(&:name)
      render :json => channel_names
    end

    def all
      owned_channels = current_account.channels.all
      shared_channels = current_account.shared_channels.all.map(&:channel)
      render json: (owned_channels + shared_channels).uniq.to_json(account_id: current_account.id)
    end

  end
end
