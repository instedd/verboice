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

  respond_to :html

  expose(:project) { current_account.projects.find(params[:project_id]) }
  expose(:external_services) { project.external_services }
  expose(:external_service)

  def create
    external_service.save
    respond_with(project, external_service)
  end

  def update
    external_service.save
    respond_with(project, external_service)
  end

  def destroy
    external_service.destroy
    respond_with(project, external_service)
  end
end
