class CallLogsController < ApplicationController
  before_filter :authenticate_account!

  # GET /applications
  def index
    @page = params[:page] || 1
    @per_page = 10
    @logs = current_account.call_logs.includes(:application).includes(:channel).order('id DESC').paginate :page => @page, :per_page => @per_page
  end

  def show
    @log = current_account.call_logs.find params[:id]
  end

  def progress
    @log = current_account.call_logs.find params[:id]
    render :layout => false
  end
end
