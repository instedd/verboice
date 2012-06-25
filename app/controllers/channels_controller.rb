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

class ChannelsController < ApplicationController
  before_filter :authenticate_account!

  # GET /channels
  def index
    @channels = current_account.channels.includes(:call_flow).all
  end

  # GET /channels/1
  def show
    @channel = current_account.channels.find(params[:id])
  end

  # GET /channels/new
  def new
    @channel = current_account.channels.new :type => params[:type]
  end

  # GET /channels/1/edit
  def edit
    @channel = current_account.channels.find(params[:id])
  end

  # POST /channels
  def create
    @channel = current_account.channels.new(params[:channel])

    if @channel.save
      redirect_to(channels_path, :notice => "Channel #{@channel.name} successfully created.")
    else
      render :action => "new"
    end
  end

  # PUT /channels/1
  def update
    @channel = current_account.channels.find(params[:id])

    if @channel.update_attributes(params[:channel])
      redirect_to(channels_path, :notice => "Channel #{@channel.name} successfully updated.")
    else
      render :action => "edit"
    end
  end

  # DELETE /channels/1
  def destroy
    @channel = current_account.channels.find(params[:id])
    @channel.destroy

    redirect_to(channels_url)
  end

  def call
    @channel = current_account.channels.find(params[:id])
    render :layout => false
  end
end
