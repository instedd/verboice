class ApplicationsController < ApplicationController
  before_filter :authenticate_account!

  # GET /applications
  # GET /applications.xml
  def index
    @applications = current_account.applications.all

    respond_to do |format|
      format.html # index.html.erb
      format.xml  { render :xml => @applications }
    end
  end

  # GET /applications/1
  # GET /applications/1.xml
  def show
    @application = current_account.applications.find(params[:id])

    respond_to do |format|
      format.html # show.html.erb
      format.xml  { render :xml => @application }
    end
  end

  # GET /applications/new
  # GET /applications/new.xml
  def new
    @application = Application.new

    respond_to do |format|
      format.html # new.html.erb
      format.xml  { render :xml => @application }
    end
  end

  # GET /applications/1/edit
  def edit
    @application = current_account.applications.find(params[:id])
  end

  # POST /applications
  # POST /applications.xml
  def create
    params[:application][:flow] = get_flow

    @application = Application.new(params[:application])
    @application.account = current_account

    respond_to do |format|
      if @application.save
        format.html { redirect_to(applications_path, :notice => "Application #{@application.name} successfully created.") }
        format.xml  { render :xml => @application, :status => :created, :location => @application }
      else
        format.html { render :action => "new" }
        format.xml  { render :xml => @application.errors, :status => :unprocessable_entity }
      end
    end
  end

  # PUT /applications/1
  # PUT /applications/1.xml
  def update
    params[:application][:flow] = get_flow

    @application = current_account.applications.find(params[:id])

    respond_to do |format|
      if @application.update_attributes(params[:application])
        format.html { redirect_to(applications_path, :notice => "Application #{@application.name} successfully updated.") }
        format.xml  { head :ok }
      else
        format.html { render :action => "edit" }
        format.xml  { render :xml => @application.errors, :status => :unprocessable_entity }
      end
    end
  end

  # DELETE /applications/1
  # DELETE /applications/1.xml
  def destroy
    @application = current_account.applications.find(params[:id])
    @application.destroy

    respond_to do |format|
      format.html { redirect_to(applications_url, :notice => "Application #{@application.name} successfully deleted.") }
      format.xml  { head :ok }
    end
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
