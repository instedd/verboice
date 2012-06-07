module Parsers
  module UserFlowNode
    class InputSetting

      include ActionView::Helpers::JavaScriptHelper
      attr_accessor :value, :variable, :step, :response

      def initialize(opts)
        options = opts.with_indifferent_access
        self.value = options['value']
        self.variable = options['variable']
        self.step = options['step']
        self.response = options['response']
      end

      def and_return_expression()
        expression
      end

      def expression()
        if step.present?
          "value_#{step}"
        elsif variable.present?
          "var_#{variable}"
        elsif value.present?
          is_numeric?(value) ? value : "'#{escape_javascript value}'"
        elsif response.present?
          "external_#{response}"
        end
      end

      def retrieve_if_needed(compiler)
        compiler.RetrieveVariable variable if variable.present?
        self
      end

      def is_numeric?(obj)
         obj.to_s.match(/\A\d+?(\.\d+)?\Z/) == nil ? false : true
      end

    end
  end
end