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
    before_filter :load_entities, :only => [:index, :show, :responses]

    def index
      data_packages = {
        data: [{
          type: "packages",
          id: @data_package.uuid
        }],
        links: {
          self: api_project_call_flow_flow_results_packages_url(@project.id, @call_flow.id)
        }
      }
      render json: data_packages
    end

    def show
      data_package_uri = api_project_call_flow_flow_results_package_url(@project.id, @call_flow.id, @data_package.uuid)

      render json: {
        data: {
          type: "packages",
          id: @data_package.uuid,
          attributes: @data_package.descriptor(data_package_uri),
          relationships: {
            responses: {
              links: {
                related: responses_api_project_call_flow_flow_results_package_url(@project.id, @call_flow.id, @data_package.uuid)
              }
            }
          }
        },
        links: {
          self: data_package_uri
        }
      }
    end

    def responses
      render json: {
        data: {
          type: "flow-results-data",
          id: @data_package.uuid,
          attributes: {
            responses: @data_package.responses
          }
        }
      }
    end

    private

    def load_entities
      @project = (current_account.projects.find(params[:project_id]) rescue nil)
      @call_flow = (current_account.call_flows.find(params[:call_flow_id]) rescue nil)

      unless @project && @call_flow && @project.id == @call_flow.project_id
        return error(404, "Call flow does not exist",
          "Call flow does not exist, does not belong to project, or you don't have permissions to access it.")
      end

      @data_package = !params[:id] ||
        (@call_flow.current_data_package && params[:id] == @call_flow.current_data_package.uuid) ? @call_flow.current_data_package : nil

      unless @data_package
        return error(404, "Call flow does not export a FLOIP package",
          "Call flow does not export a data package, probably because it's driven by a third-party app.")
      end
    end

    def error(http_status, title, detail)
      render(json: {
        errors: [{ status: http_status.to_s, title: title, detail: detail  }]
      }, status: http_status)
    end
  end
end
