class QueuedCallsController < ApplicationController
  before_filter :authenticate_account!

  def index
    @page = params[:page] || 1
    @per_page = 10
    @channel = current_account.channels.find params[:channel_id]
    @queued_calls = @channel.queued_calls.paginate :page => @page, :per_page => @per_page
  end
end
