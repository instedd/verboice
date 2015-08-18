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
  class ExternalService

    class ParseException < StandardError
    end

    def initialize(external_service=nil)
      @external_service = external_service || ::ExternalService.new
    end

    def parse(xml_string)
      begin
        xml = Nokogiri::XML.parse(xml_string)

        @external_service.name = xml.root.xpath('./name').text rescue nil

        parse_global_settings xml.root

        existing_steps_ids = @external_service.steps.pluck :id
        xml.root.xpath('./steps/step').each do |step_node|
          step = parse_step step_node
          existing_steps_ids.delete(step.id) unless step.new_record?
        end

        @external_service.steps.each do |step|
          step.mark_for_destruction if !step.new_record? && existing_steps_ids.include?(step.id)
        end

        @external_service
      rescue Exception => ex
        raise ParseException, ex.to_s
      end
    end

    private

    def parse_global_settings root
      updated_global_variables = []
      root.xpath('./global-settings/variable').each do |variable_node|
        new_variable = parse_global_variable(variable_node)
        current_variable = @external_service.global_variables.detect{|v| v.name == new_variable.name}
        new_variable.value = current_variable.value if current_variable.present?
        updated_global_variables << new_variable
      end
      @external_service.global_variables = updated_global_variables
    end

    def parse_step node
      name = node.attr('name')
      step = @external_service.steps.find_or_initialize_by_name(node.attr('name'))
      step.display_name = node.attr('display-name') if node.attr('display-name')
      step.icon = node.attr('icon') if node.attr('icon')
      step.kind = node.attr('type') if node.attr('type')
      step.script = node.xpath('script').first.text rescue nil
      step.callback_url = node.attr('callback-url') if node.attr('callback-url')
      step.async = node.attr('async') == 'true'

      response_type = node.at_xpath('./response').attr('type') rescue nil
      step.response_type = response_type if response_type

      step.variables = parse_variables node.xpath('./settings/variable')
      step.response_variables = parse_variables node.xpath('./response/variable')
      step.session_variables = parse_session_variables node.xpath('settings/session_variable')

      # FIXME: Should not save when parsing!! See how to mark for update
      step.save if !step.new_record?
      step
    end

    def parse_variables nodes
      variables = []
      nodes.each do |var_node|
        variables << parse_variable(var_node)
      end
      yield(variables) if block_given?
      variables
    end

    def parse_variable node
      ::ExternalServiceStep::Variable.new.tap do |var|
        var.name = node.attr('name')
        var.display_name = node.attr('display-name')
        var.type = node.attr('type')
      end
    end

    def parse_global_variable node
      ::ExternalService::GlobalVariable.new.tap do |var|
        var.name = node.attr('name')
        var.display_name = node.attr('display-name')
        var.type = node.attr('type')
      end
    end

    def parse_session_variables nodes
      nodes.map { |node| node.attr('name') }
    end

  end
end
