class ExternalServicesController < ApplicationController

  before_filter :authenticate_account!

  respond_to :html, :json

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

  def update_manifest
    begin
      external_service.update_manifest!
      flash[:notice] = 'Manifest successfully updated'
    rescue Exception => ex
      flash[:error] = 'Error updating manifest'
      logger.warn ex
    end
    respond_with(project, external_service, :location => project_external_services_url)
  end
end
