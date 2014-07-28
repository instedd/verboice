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

class ResourcesController < ApplicationController

  respond_to :html, :json

  expose(:project) { load_project }
  expose(:resources) do
    if params[:q].present?
      project.resources.where('name LIKE ?', "%#{params[:q]}%")
    else
      project.resources.includes(:localized_resources)
    end
  end
  expose(:resource)

  before_filter :load_project
  before_filter :check_project_admin, only: [:create, :update, :destroy]

  def index
    respond_with resource do |format|
      format.json { render :json => resources.map{|res| res.as_json(:include => :localized_resources)} }
    end
  end

  def show
    respond_with resource, :include => :localized_resources
  end

  def find
    resource = resources.find_by_guid(params[:guid])
    respond_with resource, :include => :localized_resources
  end

  def create
    resource.save
    # For some reason nested localized resources are duplicated...
    resource.reload
    respond_with resource, :include => :localized_resources
  end

  def update
    resource.save
    # For some reason nested localized resources are duplicated...
    resource.reload
    respond_with resource do |format|
      format.json { render :json => resource, :include => :localized_resources }
    end
  end

  def destroy
    resource.destroy
    render action: :index
  end

end
