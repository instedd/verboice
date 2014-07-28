class AlertsController < ApplicationController
  expose(:alerts) { current_account.alerts }
  expose(:alert)

  def index
    respond_to do |format|
      format.json { render json: alerts }
    end
  end

  def show
  end

  def dismiss
    alert.delete
    redirect_to :root
  end
end
