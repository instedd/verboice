class ApplicationsController < ApplicationController
  before_filter :authenticate_account!
  before_filter :load_application, :only => [:show, :edit, :edit_workflow, :update_workflow, :update, :destroy]


  skip_before_filter :verify_authenticity_token, :only => :save_recording

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
    p '-------------------'
    if params[:flow].is_a? Hash
      p 1
      @application.flow = Array.new
      params[:flow].each do |key, step|
        @application.flow << step
      end
    elsif params[:flow].is_a? Array
      p 2
      @application.flow = params[:flow]
    elsif params[:flow]
      p 3
      @application.flow = [params[:flow]]
    else
      p 4
      @application.flow = nil
    end
    p '-------------------'
    p params[:flow]
    p @application.flow
    p '-------------------'

    if @application.save
      respond_to do |format|
        format.html {redirect_to(application_path(@application), :notice => "Workflow for application #{@application.name} successfully updated.")}
        format.json { render(json: @application, status: 200, location: @application)}
      end
    else
      render :action => "edit_workflow"
    end
  end

  def save_recording
    p 'foooo'
    p params
    p 'bar'

    # wavfile = File.new()
    # wavfile.binmode

    File.open("recording.wav","wb") do |file|
      file.write request.body.read
    end

    # wavfile.close

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
