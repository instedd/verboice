class ApiSchedulesController < ApplicationController
  before_filter :authenticate_account!
  skip_before_filter :verify_authenticity_token

  expose(:schedules) { current_account.schedules }
  expose(:schedule) { current_account.schedules.find_by_name(params[:name]) }

  def index
    render :json => schedules
  end

  def show
    render :json => schedule
  end

  def create
    data = JSON.parse(request.raw_post).with_indifferent_access
    schedule = Schedule.from_json data
    schedule.account = current_account
    schedule.save
    head :ok
  end

  def update
    data = JSON.parse(request.raw_post).with_indifferent_access
    schedule.update_attributes! data
    head :ok
  end

  def destroy
    schedule.destroy
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