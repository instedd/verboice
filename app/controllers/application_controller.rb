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

class ApplicationController < ActionController::Base
  protect_from_forgery
  include ActionView::Helpers::TextHelper

  before_filter do
    @body_class = ['full-width']
  end

  def load_project
    return @project if @project

    project_id = params[:project_id] || params[:id]
    @project = current_account.projects.find_by_id(project_id)
    if @project
      @project_permission = "admin"
    else
      @shared_project = current_account.shared_projects.find_by_model_id(project_id)
      if @shared_project
        @project = @shared_project.project
        @project_permission = @shared_project.role
      else
        head :not_found
      end
    end
    @project
  end

  def check_project_admin
    load_project
    head :unauthorized unless @project_permission == "admin"
  end

  def load_channel
    return @channel if @channel

    channel_id = params[:channel_id] || params[:id]
    @channel = current_account.channels.find_by_id(channel_id)
    if @channel
      @channel_permission = "admin"
    else
      @shared_channel = current_account.shared_channels.find_by_model_id(channel_id)
      if @shared_channel
        @channel = @shared_channel.channel
        @channel_permission = @shared_channel.role
      else
        head :not_found
      end
    end
    @channel
  end

  def check_channel_admin
    load_channel
    head :unauthorized unless @channel_permission == "admin"
  end

  def set_fixed_width_content
    @body_class << 'fixed-width-content'
  end
end
