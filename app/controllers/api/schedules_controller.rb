# Copyright (C) 2010-2012, InSTEDD
#
# This file is part of Verboice.
#
# Verboice is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# Verboice is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with Verboice.  If not, see <http://www.gnu.org/licenses/>.
module Api
  class SchedulesController < ApiController

    expose(:project) { current_account.projects.includes(:schedules).find(params[:project_id]) }
    expose(:schedules) { project.schedules }
    expose(:schedule) { project.schedules.find_by_name(params[:name]) }

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
      if schedule.save
        render :json => schedule
      else
        render :json => errors_to_json(schedule, 'creating')
      end
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
  end
end
