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

class ProjectsController < ApplicationController
  before_filter :authenticate_account!
  before_filter :load_project, only: [:edit, :update, :destroy]
  before_filter :load_enqueue_call_fields, only: [:show, :enqueue_call]

  def index
    @projects = current_account.projects.all
  end

  def show
  end

  def new
    @project = Project.new
  end

  def edit
  end

  def create
    @project = Project.new(params[:project])
    @project.account = current_account

    if @project.save
      redirect_to(project_call_flows_path(@project), :notice => "Project #{@project.name} successfully created.")
    else
      render :action => "new"
    end
  end

  def update
    if @project.update_attributes(params[:project])
      redirect_to(project_path(@project), :notice => "Project #{@project.name} successfully updated.")
    else
      render :action => "show"
    end
  end

  def enqueue_call
    @channel = current_account.channels.find_by_id(params[:channel_id])
    if @channel
      addresses = params[:addresses].split(/\n/).map(&:strip).select(&:presence)

      options = {}
      options[:schedule_id] = params[:schedule_id] if params[:schedule_id].present?
      options[:not_before] = params[:not_before_date] if params[:not_before_date].present? && params[:not_before].present?
      options[:time_zone] = params[:time_zone] if params[:time_zone].present?
      options[:call_flow_id] = params[:call_flow_id] if params[:call_flow_id].present?
      options[:project_id] = params[:id]

      addresses.each do |address|
        @channel.call(address.strip, options)
      end
      redirect_to project_path(params[:id]), {:notice => "Enqueued calls to #{pluralize(addresses.count, 'address')} on channel #{@channel.name}"}
    else
      redirect_to project_path(params[:id]), flash: {error: 'You need to select a channel'}
    end
  end

  def destroy
    @project.destroy
    redirect_to(projects_url, :notice => "Project #{@project.name} successfully deleted.")
  end

  private

  def load_project
    @project = current_account.projects.find(params[:id])
  end

  def load_enqueue_call_fields
    @channels = current_account.channels
    @project = current_account.projects.includes(:call_flows).find(params[:id])
    @schedules = @project.schedules
    @call_flows = @project.call_flows
  end

end
