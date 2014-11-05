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
class Api::ProjectsController < ApiController
  def index
    projects = current_account.projects.includes(:call_flows, :schedules, :project_variables).sort_by { |p| p.name.downcase }.map do |project|
      project_as_json(project)
    end
    render json: projects
  end

  def show
    project = current_account.projects.find params[:id]
    render json: project_as_json(project)
  end

  private

  def project_as_json(project)
    {
      id: project.id,
      name: project.name,
      call_flows: project.call_flows.sort_by { |c| c.name.downcase }.map do |call_flow|
        {
          id: call_flow.id,
          name: call_flow.name,
        }
      end,
      schedules: project.schedules.map(&:name).sort_by { |n| n.downcase },
      contact_vars: project.project_variables.map(&:name),
    }
  end
end
