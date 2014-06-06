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

module Parsers
  module UserFlowNode
    class External < UserCommand
      attr_reader :id, :name, :call_flow
      attr_accessor :next

      def initialize call_flow, params
        @id = params['id']
        @name = params['name'] || ''
        @external_step_guid = params['external_step_guid']
        @call_flow = call_flow
        @next = params['next']
        @root_index = params['root']
        @settings = (params['settings'] || [])
        @responses = (params['responses'] || [])
      end

      def is_root?
        @root_index.present?
      end

      def root_index
        @root_index
      end

      def equivalent_flow
        service_step = @call_flow.project.external_service_steps.find_by_guid(@external_step_guid)
        service = service_step.external_service
        Compiler.parse do |compiler|
          compiler.Label @id
          compiler.StartUserStep :external_service, @id, @name, external_step_guid: @external_step_guid
          case service_step.kind
          when 'callback'
            compiler.Trace context_for %("Calling External Service #{service.name}.")
            options = {
              response_type: (service_step.response_type.present? ? service_step.response_type.to_sym : :flow),
              variables: build_variables_map(compiler, service_step),
              external_service_guid: service.guid,
            }
            options[:async] = true if service_step.async
            compiler.Callback service_step.callback_url, options
            assign_responses(compiler, service_step)
          when 'script'
            compiler.Trace context_for %("Executing External Service #{service.name}.")
            if @settings.present?
              js = 'settings = {};'
              js << (@settings.map { |setting| "settings['#{setting['name']}'] = #{InputSetting.new(setting).expression}" }.join ';')
              compiler.Js js
            end
            compiler.Js service_step.script
          end
          compiler.append @next.equivalent_flow if @next
        end
      end

      private

      def build_variables_map(compiler, service_step)
        HashWithIndifferentAccess.new.tap do |hash|
          if @settings.present?
            @settings.each do |setting|
              hash[setting['name']] = InputSetting.new(setting).expression
            end
          end
          if service_step.session_variables.present?
            service_step.session_variables.each do |session_variable|
              hash[session_variable] = session_variable
            end
          end
        end.presence
      end

      def assign_responses(compiler, service_step)
        service_step.response_variables.each do |var|
          compiler.Assign "external_#{@id}_#{var.name}", "response_#{var.name}", :try
          response = @responses.find {|r| r['name'] == var.name}
          compiler.PersistVariable response['variable'], "response_#{var.name}" if response && response['variable']
        end
      end

    end
  end
end
