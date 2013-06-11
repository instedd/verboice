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

class NuntiumChannelsController < ApplicationController
  before_filter :authenticate_account!
  before_filter :build_channel, only: [:new, :create]
  before_filter :find_channel, only: [:edit, :update]

  def index
    @channels = current_account.nuntium_channels.order(:name)
  end

  def new
  end

  def create
    if save_channel
      redirect_to nuntium_channels_path, notice: 'SMS Channel created'
    else
      render 'new'
    end
  end

  def edit
  end

  def update
    if save_channel
      redirect_to nuntium_channels_path, notice: 'SMS Channel updated'
    else
      render 'edit'
    end
  end

  def destroy
    @nuntium_channel = current_account.nuntium_channels.find(params[:id])
    @nuntium_channel.destroy
    redirect_to nuntium_channels_path, notice: 'SMS Channel deleted'
  end

  private

  def not_found
    raise ActionController::RoutingError.new('Not Found')
  end

  def build_channel
    @nuntium_channel = NuntiumChannel.new kind: params[:kind]
    @nuntium_channel.account = current_account
    @channel = @nuntium_channel.channel
    not_found if @channel.schema.nil?
  end

  def find_channel
    @nuntium_channel = current_account.nuntium_channels.find(params[:id])
    @channel = @nuntium_channel.channel
  end

  def save_channel
    @nuntium_channel.assign_attributes params[:nuntium_channel]
    @channel.assign_attributes params[:channel_data]
    @nuntium_channel.save
  end

end

