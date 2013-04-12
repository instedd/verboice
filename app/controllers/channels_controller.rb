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
    @channel_kinds = Channel.all_leaf_subclasses.map(&:kinds).flatten(1).sort_by{|x| x[0]}
    @channel_status = {} #BrokerClient.channel_status *@channels.map(&:id)
  end

  # GET /channels/1
  def show
    @channel = current_account.channels.find(params[:id])
    @errors_count = 0 #@channel.errors_count
  end

  # GET /channels/new
  def new
    if Channel.all_leaf_subclasses.map(&:name).include? params[:type]
      @channel = if params[:type] == 'Channels::TemplateBasedSip'
        params[:type].constantize.send "new_#{params[:template].underscore}_channel"
      else
        params[:type].constantize.new
      end
      @channel.account = current_account
    else
      redirect_to(channels_path, :alert => "Channel type invalid.")
    end
  end

  # GET /channels/1/edit
  def edit
    @channel = current_account.channels.find(params[:id])
  end

  # POST /channels
  def create
    if Channel.all_leaf_subclasses.map(&:name).include? params[:channel][:type]
      @channel = if params[:channel][:type] == 'Channels::TemplateBasedSip'
        params[:channel][:type].constantize.send "new_#{params[:channel][:kind].underscore}_channel"
      else
        params[:channel][:type].constantize.new
      end
      @channel.update_attributes(params[:channel])
      @channel.account = current_account

      if @channel.save
        redirect_to(channels_path, :notice => "Channel #{@channel.name} successfully created.")
      else
        render :action => "new"
      end
    else
      redirect_to(channels_path, :alert => "Channel type invalid.")
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
