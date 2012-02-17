class ApiCallQueuesController < ApplicationController
  before_filter :authenticate_account!
  skip_before_filter :verify_authenticity_token
  
  expose(:call_queues) { current_account.call_queues }
  expose(:call_queue) { current_account.call_queues.find_by_name(params[:name]) }
  
  def index
    render :json => call_queues
  end
  
  def show
    render :json => call_queue
  end
  
  def create
    data = JSON.parse(request.raw_post).with_indifferent_access
    call_queue = CallQueue.from_json data
    call_queue.account = current_account
    call_queue.save
    head :ok
  end
  
  def update
    data = JSON.parse(request.raw_post).with_indifferent_access
    call_queue.update_attributes! data 
    head :ok
  end
  
  def destroy
    call_queue.destroy
    head :ok
  end
  
  private

  def errors_to_json(errors, action)
    attrs = {
      :summary => "There were problems #{action} the channel",
      :properties => []
    }
    errors.each do |name, value|
      attrs[:properties] << { name => value }
    end
    attrs
  end
  
end