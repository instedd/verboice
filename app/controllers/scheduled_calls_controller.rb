class ScheduledCallsController < ApplicationController

  expose(:project) { load_project }
  expose(:scheduled_calls) { project.scheduled_calls }
  expose(:scheduled_call)
  expose(:call_flows) { project.call_flows }
  expose(:channels) { current_account.channels }

  before_filter :load_project, only: [:index]
  before_filter :check_project_admin, only: [:create, :update, :destroy]
  before_filter :setup_variables, only: [:index, :new]

  def index
  end

  def create
    scheduled_call.save
    render :partial => 'update'
  end

  def update
    scheduled_call.save
    render :partial => 'update'
  end

  def destroy
    scheduled_call.destroy
    redirect_to project_scheduled_calls_path(project), :notice => "Scheduled call #{scheduled_call.name} successfully deleted."
  end

  def new
    begin
      scheduled_call.filters_json = params[:filters_json]
    rescue
      scheduled_call.filters = []
    end
    render :index
  end

private

  def setup_variables
    @variables = project.project_variables.map{|x| {id: x.id, name: x.name} }
    @implicit_variables = ImplicitVariable.subclasses
    @fields = ContactsFinder.contact_fields
  end
end
