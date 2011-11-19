class ApiController < ApplicationController
  before_filter :authenticate_account!
  skip_before_filter :verify_authenticity_token

  def call
    params[:flow] = XmlParser.parse request.body if request.post?
    call_log = current_account.call params
    render :json => {:call_id => call_log.id, :state => call_log.state}
  end

  def call_state
    call_log = current_account.call_logs.where(:id => params[:id]).first
    render :json => {:call_id => call_log.id, :state => call_log.state}
  end
end
