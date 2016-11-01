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

class LocalizedResourcesController < ApplicationController

  before_filter :check_project_admin, only: [:save_recording, :save_file]
  before_filter :check_project_reader, only: [:play_recording, :play_file]

  expose(:project) { @project }
  expose(:resource) { project.resources.find(params[:resource_id]) }
  expose(:localized_resources) { resource.localized_resources }
  expose(:localized_resource)

  skip_before_filter :verify_authenticity_token, :only => [:save_recording, :save_file]

  def save_recording
    localized_resource.recorded_audio = request.body.read
    localized_resource.save
    head :ok
  end

  def play_recording
    send_data localized_resource.recorded_audio
  end

  def save_file
    localized_resource.filename = params[:filename] if params[:filename].present?
    localized_resource.uploaded_audio = request.body.read
    localized_resource.save
    head :ok
  end

  def play_file
    send_data localized_resource.uploaded_audio, :filename => localized_resource.filename
  end

end
