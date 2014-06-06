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
  before_filter :load_call_flows, only: [:new, :edit, :create]
  before_filter :load_channel, only: [:show, :edit, :update, :destroy, :call]
  before_filter :check_channel_admin, only: [:update, :destroy]

  # GET /channels
  def index
    @channels = current_account.channels.includes(:call_flow).all
    @shared_channels = current_account.shared_channels.all
    all_channels = @channels + @shared_channels.map(&:channel)

    @channel_kinds = Channel.all_leaf_subclasses.map(&:kinds).flatten(1).sort_by{|x| x[0]}

    grouped_channels = all_channels.each_with_object(Hash.new { |h,k| h[k] = [] }) { |ch, h| h[ch.broker] << ch.id }
    @channel_status = BrokerClient.channel_status(grouped_channels) rescue {}

    @all_channels = @channels.map { |c| [c, nil] } +
                    @shared_channels.map { |sc| [sc.channel, sc.role] }
  end

  # GET /channels/1
  def show
    @errors_count = 0 #@channel.errors_count
    if @channel.project
      @project = current_account.find_project_by_id(@channel.project.id)
    end
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
    if @channel.update_attributes(params[:channel])
      redirect_to(channels_path, :notice => "Channel #{@channel.name} successfully updated.")
    else
      load_call_flows
      render :action => "edit"
    end
  end

  # DELETE /channels/1
  def destroy
    @channel.destroy

    redirect_to(channels_url)
  end

  def call
    render :layout => false
  end

  private

  def load_call_flows
    t = Project.arel_table
    shared_project_ids = current_account.shared_projects.pluck(:model_id)
    @projects = Project.where(t[:account_id].eq(current_account.id).or(t[:id].in(shared_project_ids))).includes(:call_flows)
  end
end
