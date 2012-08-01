class LocalizedResourcesController < ApplicationController

  expose(:project) { current_account.projects.find(params[:project_id]) }
  expose(:resource) { project.resources.find(params[:resource_id]) }
  expose(:localized_resources) { resource.localized_resources }
  expose(:localized_resource)

  skip_before_filter :verify_authenticity_token, :only => :save_recording

  def save_recording
    localized_resource.audio = request.body.read
    localized_resource.save
    head :ok
  end

  def play_recording
    send_data localized_resource.audio
  end

end