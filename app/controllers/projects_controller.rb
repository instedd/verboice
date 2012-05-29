class ProjectsController < ApplicationController
  before_filter :authenticate_account!
  before_filter :load_project, :only => [ :show, :edit, :update, :destroy ]

  def index
    @projects = current_account.projects.all
  end

  def show
  end

  def new
    @project = Project.new
  end

  def edit
  end

  def create
    @project = Project.new(params[:project])
    @project.account = current_account

    if @project.save
      redirect_to(project_call_flows_path(@project), :notice => "Project #{@project.name} successfully created.")
    else
      render :action => "new"
    end
  end

  def update
    if @project.update_attributes(params[:project])
      redirect_to(project_path(@project), :notice => "Project #{@project.name} successfully updated.")
    else
      render :action => "show"
    end
  end

  def destroy
    @project.destroy
    redirect_to(projects_url, :notice => "Project #{@project.name} successfully deleted.")
  end

  private

  def load_project
    @project = current_account.projects.find(params[:id])
  end

end
