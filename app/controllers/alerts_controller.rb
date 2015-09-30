class AlertsController < ApplicationController
  before_filter :ensure_account
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

  private

  def ensure_account
    return head :not_found unless current_account
  end
end
