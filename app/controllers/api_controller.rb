class ApiController < ApplicationController
  before_filter :authenticate_account!
  skip_before_filter :verify_authenticity_token

  def call
    params[:flow] = Parsers::Xml.parse request.body if request.post?
    call_log = current_account.call params
    render :json => {:call_id => call_log.id, :state => call_log.state}
  end

  def call_redirect
    options = {}
    if request.post?
      options[:flow] = Parsers::Xml.parse request.body
    elsif params[:application_id]
      if not current_account.applications.exists? params[:applications_id]
        return render :status => 404
      end
      options[:applications_id] = params[:applications_id]
    elsif params[:callback_url]
      options[:callback_url] = params[:callback_url]
    else
      return render :status => 400
    end

    channel = CallLog.find(params[:id]).channel
    channel.broker_client.redirect options

    render :text => 'OK'
  end

  def call_state
    call_log = current_account.call_logs.where(:id => params[:id]).first
    render :json => {:call_id => call_log.id, :state => call_log.state}
  end
end
