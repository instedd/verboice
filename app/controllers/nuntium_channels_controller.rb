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
  before_filter :find_channel, only: [:edit, :update, :destroy]

  def index
    @channels = current_account.nuntium_channels
  end

  def new
    add_breadcrumb 'New', new_nuntium_channel_path(:kind => params[:kind])
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
    @nuntium_channel.destroy
    redirect_to nuntium_channels_path, notice: 'SMS Channel deleted'
  end

  private

  def not_found
    raise ActionController::RoutingError.new('Not Found')
  end

  def build_channel
    @channel = Pigeon::NuntiumChannel.new kind: params[:kind]
    @channel.generate_name!
    @channel_schema = @channel.schema
    not_found if @channel_schema.nil?

    @nuntium_channel = NuntiumChannel.new
    @nuntium_channel.account = current_account
    @nuntium_channel.channel_name = @channel.name
  end

  def find_channel
    @nuntium_channel = current_account.nuntium_channels.find(params[:id])
    @channel = Pigeon::NuntiumChannel.find(@nuntium_channel.channel_name)
    @channel_schema = @channel.schema
  end

  def save_channel
    @nuntium_channel.assign_attributes params[:nuntium_channel]
    @channel.assign_attributes params[:channel_data]
    begin
      @nuntium_channel.transaction do
        @nuntium_channel.save!
        @channel.save!
      end
      true
    rescue ActiveRecord::RecordInvalid, Pigeon::ChannelInvalid
      false
    end
  end

end

