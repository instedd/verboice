# Copyright (C) 2010-2012, InSTEDD
# 
# This file is part of Verboice.
# 
# Verboice is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
# 
# Verboice is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with Verboice.  If not, see <http://www.gnu.org/licenses/>.

require 'csv'

class CallFlowsController < ApplicationController

  before_filter :authenticate_account!
  before_filter :load_call_flow_and_project, :only => [
    :show, :edit, :edit_workflow, :update_workflow, :update, :destroy, :play_recording, :save_recording, :play_result, :import_call_flow, :export_call_flow
  ]
  before_filter :load_recording_data, :only => [:play_recording, :save_recording, :play_result]

  skip_before_filter :verify_authenticity_token, :only => :save_recording

  def show
    respond_to do |format|
      format.csv do
        csv = CSV.generate({ :col_sep => ','}) do |csv|

          steps = @call_flow.step_names
          ids = steps.keys
          header = ['Call ID', 'Phone Number', 'Start Time', 'End Time']
          csv << header + steps.values
          @call_flow.call_logs.includes(:traces).each do |call_log|
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
                # This should be fixed when the call flow stores it's different flow versions.
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

  def index
    @project = current_account.projects.includes(:call_flows).find(params[:project_id])
    @call_flows = @project.call_flows
  end

  def new
    @call_flow = CallFlow.new
    @call_flow.project = current_account.projects.find(params[:project_id])
  end

  def create
    @project = current_account.projects.includes(:call_flows).find(params[:project_id])
    @call_flow = @project.call_flows.create(params[:call_flow])

    if @call_flow.save
      redirect_to edit_workflow_project_call_flow_path(@project, @call_flow), notice: 'Call flow was successfully created.'
    else
      render action: "new"
    end
  end

  def destroy
    @call_flow.destroy
    redirect_to project_call_flows_path(@project)
  end

  def edit
    @variables = current_account.distinct_variables
  end

  def update
    if @call_flow.update_attributes(params[:call_flow])
        redirect_to(edit_project_call_flow_path(@project, @call_flow), :notice => "Call Flow #{@call_flow.name} successfully updated.")
    else
      render :action => "edit"
    end
  end

  def update_workflow
    @call_flow.user_flow = JSON.parse params[:flow]
    if @call_flow.save
        redirect_to(edit_workflow_project_call_flow_path(@project, @call_flow), :notice => "Call Flow #{@call_flow.name} successfully updated.")
    else
      render :action => "edit_workflow"
    end
  end

  def edit_workflow
    @variables = current_account.distinct_variables
  end

  def import_call_flow
    if params[:vrb].blank?
      redirect_to({ :action => :show }, :flash => { :alert => 'No file found' })
    else
      begin
        extension = File.extname params[:vrb].original_filename
        case extension
        when '.vrb'
          @call_flow.user_flow = YAML::load File.read(params[:vrb].tempfile.path)
        when '.vrz'
          VrzContainer.for(@call_flow).import params[:vrb].tempfile.path
        else
          raise 'Invalid extension'
        end
        @call_flow.save!
        redirect_to({ :action => :edit }, {:notice => "Call Flow #{@call_flow.name} successfully updated."})
      rescue Exception => ex
        redirect_to({:action => :edit}, :flash => {:error => "Invalid file: #{ex}"})
      end
    end
  end

  def export_call_flow
    if params[:export_audios]
      file = Tempfile.new(@call_flow.id.to_s)
      begin
        VrzContainer.for(@call_flow).export file.path
      ensure
        file.close
      end
      send_file file.path, :x_sendfile => true, :filename => "#{@call_flow.id}.vrz"
    else
      send_data @call_flow.user_flow.to_yaml, :filename => "#{@call_flow.id}.vrb"
    end
  end

  def play_recording
    send_file @recording_manager.recording_path_for(@step_id, @message), :x_sendfile=>true
  end

  def save_recording
    @recording_manager.save_recording_for(@step_id, @message) do |out|
      out.write request.body.read
    end
    render text: @step_id
  end

  private

  def load_recording_data
    @step_id = params[:step_id]
    @message = params[:message]
    @recording_manager = RecordingManager.for(@call_flow)
  end

  def load_call_flow_and_project
    @project = current_account.projects.includes(:call_flows).find(params[:project_id])
    @call_flow = @project.call_flows.find(params[:id])
  end
end
