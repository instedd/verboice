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
module Api::FlowResults
  class PackagesController < ApiController
    def index
      project_id = params[:project_id]
      call_flow_id = params[:call_flow_id]
      call_flow = CallFlow.where(:project_id => project_id, :id => call_flow_id).first

      unless call_flow
        return error(404, "Call flow does not exist",
          "Call flow does not exist, does not belong to project, or you don't have permissions to access it.")
      end

      unless call_flow.current_data_package
        return error(404, "Call flow does not export a FLOIP package",
          "Call flow does not export a data package, probably because it's driven by a third-party app.")
      end

      data_package_uuid = call_flow.current_data_package.uuid
      data_package_uri = api_project_call_flow_flow_results_package_url(project_id, call_flow_id, data_package_uuid)
      data_packages = {
        data: [
          type: "packages",
          id: data_package_uuid
        ],
        links: {
          self: data_package_uri
        }
      }
      render json: data_packages
    end

    private

    def error(http_status, title, detail)
      render(json: {
        errors: [{ status: http_status.to_s, title: title, detail: detail  }]
      }, status: http_status)
    end
  end
end
