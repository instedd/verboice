class SchedulesController < ApplicationController
  respond_to :html
  expose(:schedules) { current_account.schedules }
  expose(:schedule)

  def create
    if schedule.save
      redirect_to schedules_path, :notice => "Schedule #{schedule.name} successfully created."
    else
      respond_with schedule
    end
  end

  def update
    if schedule.save
      redirect_to schedules_path, :notice => "Schedule #{schedule.name} successfully updated."
    else
      respond_with schedule
    end
  end

  def destroy
    schedule.destroy
    redirect_to schedules_path, :notice => "Schedule #{schedule.name} successfully deleted."
  end
end