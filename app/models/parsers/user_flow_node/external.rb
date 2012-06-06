module Parsers
  module UserFlowNode
    class External < UserCommand
      attr_reader :id, :name, :call_flow
      attr_accessor :next

      def initialize call_flow, params
        @id = params['id']
        @name = params['name'] || ''
        @external_step_id = params['external_step_id']
        @call_flow = call_flow
        @next = params['next']
        @root_index = params['root']
        @settings = params['settings']
        @responses = params['responses']
      end

      def is_root?
        @root_index.present?
      end

      def root_index
        @root_index
      end

      def equivalent_flow
        service_step = ExternalServiceStep.find(@external_step_id)
        service = service_step.external_service

        Compiler.parse do |compiler|
          compiler.Label @id
          compiler.Assign "current_step", @id
          compiler.Trace context_for %("Calling External Service #{service.name}.")
          compiler.Callback service_step.callback_url, {:response_type => service_step.response_type.to_sym, :variables => build_variables_map(compiler), :external_service_id => service.id}
          assign_responses(compiler, service_step)
          compiler.append @next.equivalent_flow if @next
        end
      end

      private

      def build_variables_map(compiler)
        return nil unless @settings.present?
        HashWithIndifferentAccess.new.tap do |map|
          @settings.each do |setting|
            if setting['step'].present?
              map[setting['name']] = "value_#{setting['step']}"
            elsif setting['variable'].present?
              compiler.RetrieveVariable setting['variable']
              map[setting['name']] = setting['variable']
            elsif setting['value'].present?
              map[setting['name']] = "'#{setting['value']}'"
            end
          end
        end
      end

      def assign_responses(compiler, service_step)
        service_step.response_variables.each do |var|
          compiler.Assign "external_#{@id}_#{var.name}", "response_#{var.name}", :try
          response = @responses.find {|r| r['name'] == var.name}
          compiler.PersistVariable response['variable'], "response_#{var.name}" if response && response['variable']
        end
      end

      # def build_responses_map(compiler)
      #   return nil unless @responses.present?
      #   HashWithIndifferentAccess.new.tap do |map|
      #     @responses.each do |response|
      #       if response['variable']
      #         map[response['name']] = response['variable']
      #       end
      #     end
      #   end
      # end

    end
  end
end
