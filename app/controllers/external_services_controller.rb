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

class ExternalServicesController < ApplicationController

  before_filter :authenticate_account!

  respond_to :html, :json

  expose(:project) { load_project }
  expose(:external_services) { project.external_services }
  expose(:external_service)

  before_filter :load_project
  before_filter :check_project_admin, only: [:create, :update, :destroy, :update_manifest]

  def create
    external_service.save
    if request.xhr?
      render :partial => "box_content", :locals => { :external_service => external_service, :expanded => external_service.errors.any?}
    else
      render :action => "index"
    end
  end

  def update
    external_service.save
    if request.xhr?
      render :partial => "box_content", :locals => { :external_service => external_service, :expanded => external_service.errors.any?}
    else
      render :action => "index"
    end
  end

  def destroy
    external_service.clean_call_flows
    external_service.destroy
    render :action => "index"
  end

  def update_manifest
    begin
      external_service.update_manifest!
      flash[:notice] = 'Manifest successfully updated'
    rescue Parsers::ExternalService::ParseException => pex
      flash[:error] = 'Error updating manifest: Invalid manifest format'
      Rails.logger.warn "Parsing error updating manifest for service #{external_service.try(:id)}: #{pex}"
    rescue RestClient::Exception => rex
      flash[:error] = 'Error updating manifest: Cannot download manifest'
      Rails.logger.warn "Connection error updating manifest for service #{external_service.try(:id)}: #{rex}"
    rescue Exception => ex
      flash[:error] = 'Error updating manifest'
      Rails.logger.warn "Error updating manifest for service #{external_service.try(:id)}: #{ex}"
    end
    if request.xhr?
      render :partial => "box_content", :locals => { :external_service => external_service, :expanded => true}
    else
      render :action => "index"
    end
  end
end
