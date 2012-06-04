module Parsers
  class ExternalService

    def initialize(external_service=nil)
      @external_service = external_service || ::ExternalService.new
    end

    def parse(xml_string)
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
      attributes = {
        name: node.attr('name'),
        display_name: node.attr('display-name'),
        icon: node.attr('icon'),
        kind: node.attr('type'),
        response_type: node.attr('response-type'),
        callback_url: node.attr('callback-url')
      }

      name = node.attr('name')
      step = @external_service.steps.find_or_initialize_by_name(attributes[:name])
      step.display_name = node.attr('display-name') if node.attr('display-name')
      step.icon = node.attr('icon') if node.attr('icon')
      step.kind = node.attr('type') if node.attr('type')
      step.response_type = node.attr('response-type') if node.attr('response-type')
      step.callback_url = node.attr('callback-url') if node.attr('callback-url')

      step.variables = parse_variables node.xpath('./settings/variable')
      step.response_variables = parse_variables node.xpath('./settings/response-variable')

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

  end
end