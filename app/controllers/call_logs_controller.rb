class CallLogsController < ApplicationController
  before_filter :authenticate_account!

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
  end

  def play_result
    log = current_account.call_logs.find params[:id]
    send_file RecordingManager.for(log).result_path_for(params[:key]), :x_sendfile => true
  end

end
