class LocalizedResourcesController < ApplicationController

  expose(:project) { current_account.projects.find(params[:project_id]) }
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