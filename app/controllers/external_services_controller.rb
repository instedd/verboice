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
