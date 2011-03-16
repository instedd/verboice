class CallLogsController < ApplicationController
  before_filter :authenticate_account!

  # GET /applications
  # GET /applications.xml
  def index
    @logs = current_account.call_logs.all

    respond_to do |format|
      format.html # index.html.erb
      format.xml  { render :xml => @logs }
    end
  end
end
