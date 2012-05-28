require 'csv'

class ProjectsController < ApplicationController
  before_filter :authenticate_account!
  before_filter :load_project, :only => [
    :show, :edit, :edit_workflow, :update_workflow, :update, :destroy, :play_recording, :save_recording, :play_result, :import_call_flow, :export_call_flow
  ]
  before_filter :load_recording_data, :only => [:play_recording, :save_recording, :play_result]

  skip_before_filter :verify_authenticity_token, :only => :save_recording

  # GET /projects
  def index
    @projects = current_account.projects.all
  end

  # GET /projects/1
  # Trace.create! project_id: @project_id, step_id: @step_id, step_name: @step_name, call_id: session.call_id, result: session.eval(@expression)
  def show
    respond_to do |format|
      format.html
      format.csv do
        csv = CSV.generate({ :col_sep => ','}) do |csv|

          steps = @project.step_names
          ids = steps.keys
          header = ['Call ID', 'Phone Number', 'Start Time', 'End Time']
          csv << header + steps.values
          @project.call_logs.includes(:traces).each do |call_log|
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
                # This should be fixed when the project stores it's different flow versions.
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

  # GET /projects/new
  def new
    @project = Project.new
  end

  # GET /projects/1/edit
  def edit
  end

  # POST /projects
  def create
    @project = Project.new(params[:project])
    @project.account = current_account

    if @project.save
      redirect_to(edit_workflow_project_path(@project), :notice => "Project #{@project.name} successfully created.")
    else
      render :action => "new"
    end
  end

  # PUT /projects/1
  def update
    if @project.update_attributes(params[:project])
      redirect_to(project_path(@project), :notice => "Project #{@project.name} successfully updated.")
    else
      render :action => "edit"
    end
  end

  def edit_workflow
    @variables = current_account.distinct_variables
  end

  def update_workflow
    @variables = current_account.distinct_variables
    @project.user_flow = JSON.parse params[:flow]

    if @project.save
      respond_to do |format|
        format.html { redirect_to(edit_workflow_project_path(@project), :notice => "Workflow for project #{@project.name} successfully updated.")}
        format.json { render(json: @project, status: 200, location: @project)}
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

  # DELETE /projects/1
  def destroy
    @project.destroy
    redirect_to(projects_url, :notice => "Project #{@project.name} successfully deleted.")
  end

  def import_call_flow
    if params[:vrb].blank?
      redirect_to({ :action => :show }, :flash => { :alert => 'No file found' })
    else
      begin
        extension = File.extname params[:vrb].original_filename
        case extension
        when '.vrb'
          @project.user_flow = YAML::load File.read(params[:vrb].tempfile.path)
        when '.vrz'
          VrzContainer.for(@project).import params[:vrb].tempfile.path
        else
          raise 'Invalid extension'
        end
        @project.save!
        redirect_to({ :action => :show }, {:notice => "Project #{@project.name} successfully updated."})
      rescue Exception => ex
        redirect_to({:action => :show}, :flash => {:error => 'Invalid file'})
      end
    end
  end

  def export_call_flow
    if params[:export_audios]
      file = Tempfile.new(@project.id.to_s)
      begin
        VrzContainer.for(@project).export file.path
      ensure
        file.close
      end
      send_file file.path, :x_sendfile => true, :filename => "#{@project.id}.vrz"
    else
      send_data @project.user_flow.to_yaml, :filename => "#{@project.id}.vrb"
    end
  end

  private

  def load_recording_data
    @step_id = params[:step_id]
    @message = params[:message]
    @recording_manager = RecordingManager.for(@project)
  end

  def load_project
    @project = current_account.projects.find(params[:id])
  end

  def get_flow
    return nil unless params[:project][:flow].present?

    ret = params[:project][:flow].map do |props|
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
