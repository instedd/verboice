class ResourcesController < ApplicationController

  respond_to :html, :json

  expose(:project) { current_account.projects.find(params[:project_id]) }
  expose(:resources) { project.resources }
  expose(:resource) { resources.find_by_guid(params[:id])}

  def index
    if params[:q].present?
      result = resources.where('name LIKE ?', "%#{params[:q]}%")
    else
      result = resources
    end
    respond_with result
  end

  def show
    respond_with resource, :include => :localized_resources
  end

  def create
    resource.save
    respond_with resource, :include => :localized_resources
  end

  def update
    resource.save
    respond_with resource do |format|
      format.json { render :json => resource, :include => :localized_resources }
    end
  end

end