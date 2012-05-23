class CallLogsController < ApplicationController
  before_filter :authenticate_account!
  include ActionView::Helpers::TextHelper

  def index
    @page = params[:page] || 1
    @search = params[:search]
    @per_page = 10
    @logs = current_account.call_logs.includes(:project).includes(:channel).order('id DESC')
    @logs = @logs.search @search, :account => current_account if @search.present?
    @logs = @logs.paginate :page => @page, :per_page => @per_page
  end

  def show
    @log = current_account.call_logs.find params[:id]
  end

  def progress
    @log = current_account.call_logs.find params[:id]
    render :layout => false
  end

  def queued
    @page = params[:page] || 1
    @per_page = 10
    @calls = current_account.queued_calls.includes(:channel).includes(:call_log).includes(:schedule).order('id DESC')
    @calls = @calls.paginate :page => @page, :per_page => @per_page

    @channels = current_account.channels
    @schedules = current_account.schedules
    @projects = current_account.projects
  end

  def enqueue
    @channel = current_account.channels.find_by_id(params[:channel_id])
    if @channel
      addresses = params[:addresses].split(/\n/).map(&:strip).select(&:presence)
      options = {}
      options[:schedule_id] = params[:schedule_id] if params[:schedule_id].present?
      options[:not_before] = params[:not_before] if params[:not_before].present?
      options[:project_id] = params[:project_id]
      addresses.each do |address|
        @channel.call(address.strip, options)
      end
      redirect_to queued_call_logs_path, {:notice => "Enqueued calls to #{pluralize(addresses.count, 'address')} on channel #{@channel.name}"}
    else
      redirect_to queued_call_logs_path, :flash => { :error => 'You need to select a channel' }
    end
  end

  def play_result
    log = current_account.call_logs.find params[:id]
    send_file RecordingManager.for(log).result_path_for(params[:key]), :x_sendfile => true
  end

end
