class ApplicationsController < ApplicationController
  before_filter :authenticate_account!

  # GET /applications
  def index
    @applications = current_account.applications.all
  end

  # GET /applications/1
  def show
    @application = current_account.applications.find(params[:id])
  end

  # GET /applications/new
  def new
    @application = Application.new
  end

  # GET /applications/1/edit
  def edit
    @application = current_account.applications.find(params[:id])
  end

  # POST /applications
  def create
    params[:application][:flow] = get_flow

    @application = Application.new(params[:application])
    @application.account = current_account

    if @application.save
      redirect_to(applications_path, :notice => "Application #{@application.name} successfully created.")
    else
      render :action => "new"
    end
  end

  # PUT /applications/1
  def update
    params[:application][:flow] = get_flow

    @application = current_account.applications.find(params[:id])

    if @application.update_attributes(params[:application])
      redirect_to(applications_path, :notice => "Application #{@application.name} successfully updated.")
    else
      render :action => "edit"
    end
  end

  # DELETE /applications/1
  def destroy
    @application = current_account.applications.find(params[:id])
    @application.destroy

    redirect_to(applications_url, :notice => "Application #{@application.name} successfully deleted.")
  end

  private

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
