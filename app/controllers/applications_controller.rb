require 'csv'

class ApplicationsController < ApplicationController
  before_filter :authenticate_account!
  before_filter :load_application, :only => [:show, :edit, :edit_workflow, :update_workflow, :update, :destroy, :play_recording, :save_recording]
  before_filter :load_recording_data, :only => [:play_recording, :save_recording]

  skip_before_filter :verify_authenticity_token, :only => :save_recording

  # GET /applications
  def index
    @applications = current_account.applications.all
  end

  # GET /applications/1
  def show
    respond_to do |format|
      format.html
      format.csv do
        csv = CSV.generate({ :col_sep => ','}) do |csv|
          csv << ['Call ID', 'Address', 'Step', 'TimeStamp', 'Description']
          # Trace.create! application_id: @application_id, step_id: @step_id, call_id: session.call_id, result: session.eval(@expression)
          @application.traces.includes(:call_log).each do |trace|
            csv << [
              trace.call_id,
              trace.call_log.address,
              trace.step_id,
              trace.created_at,
              trace.result
            ]
          end
        end
        render :text =>  csv
      end
    end
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
    @application.user_flow = JSON.parse params[:flow]

    if @application.save
      respond_to do |format|
        format.html { redirect_to(edit_workflow_application_path(@application), :notice => "Workflow for application #{@application.name} successfully updated.")}
        format.json { render(json: @application, status: 200, location: @application)}
      end
    else
      render :action => "edit_workflow"
    end
  end

  def play_recording
    send_file @recording_manager.get_recording_path_for(@step_id, @message), :x_sendfile=>true
  end

  def save_recording
    @recording_manager.save_recording_for(@step_id, @message) do |out|
      out.write request.body.read
    end
  end

  # DELETE /applications/1
  def destroy
    @application.destroy
    redirect_to(applications_url, :notice => "Application #{@application.name} successfully deleted.")
  end

  private

  def load_recording_data
    @step_id = params[:step_id]
    @message = params[:message]
    @recording_manager = RecordingManager.for(@application)
  end

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
