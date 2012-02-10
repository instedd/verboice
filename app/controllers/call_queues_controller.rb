class CallQueuesController < ApplicationController
  respond_to :html
  expose(:call_queues) { current_account.call_queues }
  expose(:call_queue)

  def create
    if call_queue.save
      redirect_to call_queues_path, :notice => "Call queue #{call_queue.name} successfully created."
    else
      respond_with call_queue
    end
  end

  def update
    if call_queue.save
      redirect_to call_queues_path, :notice => "Call queue #{call_queue.name} successfully updated."
    else
      respond_with call_queue
    end
  end

  def destroy
    call_queue.destroy
    redirect_to call_queues_path, :notice => "Call queue #{call_queue.name} successfully deleted."
  end
end