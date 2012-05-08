require 'csv'

class ApplicationsController < ApplicationController
  before_filter :authenticate_account!
  before_filter :load_application, :only => [:show, :edit, :edit_workflow, :update_workflow, :update, :destroy, :play_recording, :save_recording, :play_result]
  before_filter :load_recording_data, :only => [:play_recording, :save_recording, :play_result]

  skip_before_filter :verify_authenticity_token, :only => :save_recording

  # GET /applications
  def index
    @applications = current_account.applications.all
  end

  # GET /applications/1
  # Trace.create! application_id: @application_id, step_id: @step_id, step_name: @step_name, call_id: session.call_id, result: session.eval(@expression)
  def show
    respond_to do |format|
      format.html
      format.csv do
        csv = CSV.generate({ :col_sep => ','}) do |csv|

          steps = @application.step_names
          ids = steps.keys
          header = ['Call ID', 'Phone Number', 'Start Time', 'End Time']
          csv << header + steps.values
          @application.call_logs.includes(:traces).each do |call_log|
            line = []
            line << call_log.id
            line << call_log.address
            line << call_log.started_at
            line << call_log.finished_at
            call_log.traces.each do |trace|
              begin
                line[ids.index(trace.step_id.to_i) + header.size] = trace.result
              rescue Exception => e
                # If the Trace belongs to a deleted step, there is no way to represent it.
                # This should be fixed when the application stores it's different flow versions.
                # For now, the trace is ignored
              end
            end
            csv << line
          end
        end
        render :text => csv
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

  def edit_workflow
    @variables = current_account.distinct_variables
  end

  def update_workflow
    @variables = current_account.distinct_variables
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
    send_file @recording_manager.recording_path_for(@step_id, @message), :x_sendfile=>true
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
