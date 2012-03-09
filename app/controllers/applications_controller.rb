class ApplicationsController < ApplicationController
  before_filter :authenticate_account!
  before_filter :load_application, :only => [:show, :edit, :edit_workflow, :update_workflow, :update, :destroy]

  # GET /applications
  def index
    @applications = current_account.applications.all
  end

  # GET /applications/1
  def show
  end

  # GET /applications/new
  def new
    @application = Application.new
  end

  # GET /applications/1/edit
  def edit
  end

  def edit_workflow
  end

  # POST /applications
  def create
    @application = Application.new(params[:application])
    @application.account = current_account

    if @application.save
      redirect_to(edit_workflow_application_path(@application), :notice => "Application #{@application.name} successfully created.")
    else
      render :action => "new"
    end
  end

  # PUT /applications/1
  def update
    if @application.update_attributes(params[:application])
      redirect_to(application_path(@application), :notice => "Application #{@application.name} successfully updated.")
    else
      render :action => "edit"
    end
  end

  def update_workflow
    @application.flow = params[:flow]
    p params[:flow]
    @application.flow= [@application.flow] if @application.flow.is_a Hash

    if @application.save
      redirect_to(application_path(@application), :notice => "Workflow for application #{@application.name} successfully updated.")
    else
      render :action => "edit_workflow"
    end
  end

  # DELETE /applications/1
  def destroy
    @application.destroy
    redirect_to(applications_url, :notice => "Application #{@application.name} successfully deleted.")
  end

  private

  def load_application
    @application = current_account.applications.find(params[:id])
  end

  def get_flow
    return nil unless params[:application][:flow].present?

    ret = params[:application][:flow].map do |props|
      name = props[:name].downcase.to_sym
      args = props.reject { |k, v| k.to_sym == :name}
      case args.length
      when 0 then name
      when 1 then {name => args.first[1]}
      else {name => args.to_hash}
      end
    end
    ret
  end
end
